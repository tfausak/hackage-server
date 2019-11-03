{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, RecordWildCards,
             TemplateHaskell, TypeFamilies, FlexibleInstances, MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Reporting
-- Copyright   :  (c) David Waern 2008
-- License     :  BSD-like
--
-- Maintainer  :  david.waern@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Anonymous build report data structure, printing and parsing
--
-----------------------------------------------------------------------------
module Distribution.Server.Features.BuildReports.BuildReport (
    BuildReport(..),
    InstallOutcome(..),
    Outcome(..),

    -- * parsing and pretty printing
    parse,
    show,

    affixTimestamp,

    BuildReport_v0,
  ) where

import CabalCompat.Package
         ( PackageIdentifier(..) )
import Distribution.Types.GenericPackageDescription
         ( FlagName, unFlagName )
import Distribution.System
         ( OS, Arch )
import Distribution.Compiler
         ( CompilerId )
import qualified CabalCompat.Text as Text
import qualified Distribution.FieldGrammar as F
import qualified Distribution.Fields as F
import qualified Distribution.Compat.Newtype as Newtype
import qualified Distribution.CabalSpecVersion as Spec
import qualified Data.ByteString.Char8 as B
import Distribution.Server.Framework.Instances ()
import Distribution.Server.Framework.MemSize

import qualified CabalCompat.ReadP as Parse
import qualified Text.PrettyPrint.HughesPJ as Disp
         ( Doc, text, char, (<>) )
import Text.PrettyPrint.HughesPJ
         ( (<+>), render )
import Data.Serialize as Serialize
         ( Serialize(..) )
import Data.SafeCopy
         ( SafeCopy(..), deriveSafeCopy, extension, base, Migrate(..) )
import Test.QuickCheck
         ( Arbitrary(..), elements, oneof )
import Text.StringTemplate ()
import Text.StringTemplate.Classes
         ( SElem(..), ToSElem(..) )

import Data.Char as Char
         ( isAlpha, isAlphaNum )
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as Map
import Data.Time
         ( UTCTime, getCurrentTime )
import Data.Typeable
         ( Typeable )
import Control.Applicative
import Control.Monad

import Prelude hiding (show, read)
import qualified Prelude


data BuildReport
   = BuildReport {
    -- | The package this build report is about
    package         :: PackageIdentifier,

    -- | The time at which the report was uploaded
    time            :: Maybe UTCTime,

    -- | Whether the client was generating documentation for upload
    docBuilder      :: Bool,

    -- | The OS and Arch the package was built on
    os              :: OS,
    arch            :: Arch,

    -- | The Haskell compiler (and hopefully version) used
    compiler        :: CompilerId,

    -- | The uploading client, ie cabal-install-x.y.z
    client          :: PackageIdentifier,

    -- | Which configurations flags we used
    flagAssignment  :: [(FlagName,Bool)],
    -- TODO: this is the pre-Cabal-2.2 'FlagAssignment' type;
    --       consider changing this to the new opaque 'FlagAssignment' type at some point
    --       (which will have implications for the safecopy serialisation)

    -- | Which dependent packages we were using exactly
    dependencies    :: [PackageIdentifier],

    -- | Did installing work ok?
    installOutcome  :: InstallOutcome,

    --   Which version of the Cabal library was used to compile the Setup.hs
--    cabalVersion    :: Version,

    --   Which build tools we were using (with versions)
--    tools      :: [PackageIdentifier],

    -- | Configure outcome, did configure work ok?
    docsOutcome     :: Outcome,

    -- | Configure outcome, did configure work ok?
    testsOutcome    :: Outcome
  }
  deriving (Eq, Typeable, Show)

data InstallOutcome
   = PlanningFailed
   | DependencyFailed PackageIdentifier
   | DownloadFailed
   | UnpackFailed
   | SetupFailed
   | ConfigureFailed
   | BuildFailed
   | InstallFailed
   | InstallOk
   deriving (Eq, Ord, Show)

data Outcome = NotTried | Failed | Ok deriving (Eq, Ord, Show)

-- ------------------------------------------------------------
-- * External format
-- ------------------------------------------------------------

buildReportFieldGrammar :: (F.FieldGrammar fg, Applicative (fg BuildReport)) => fg BuildReport BuildReport
buildReportFieldGrammar = BuildReport
  <$> F.uniqueField (makeFieldName "package") (makeLens package (\ r x -> r { package = x }))
  <*> F.optionalField (makeFieldName "time") (makeLens time (\ r x -> r { time = x }))
  <*> F.booleanFieldDef (makeFieldName "doc-builder") (makeLens docBuilder (\ r x -> r { docBuilder = x })) False
  <*> F.uniqueField (makeFieldName "os") (makeLens os (\ r x -> r { os = x }))
  <*> F.uniqueField (makeFieldName "arch") (makeLens arch (\ r x -> r { arch = x }))
  <*> F.uniqueField (makeFieldName "compiler") (makeLens compiler (\ r x -> r { compiler = x }))
  <*> F.uniqueField (makeFieldName "client") (makeLens client (\ r x -> r { client = x }))
  <*> F.optionalFieldDefAla (makeFieldName "flags") Flags (makeLens flagAssignment (\ r x -> r { flagAssignment = x })) []
  <*> F.optionalFieldDefAla (makeFieldName "dependencies") Dependencies (makeLens dependencies (\ r x -> r { dependencies = x })) []
  <*> F.uniqueField (makeFieldName "install-outcome") (makeLens installOutcome (\ r x -> r { installOutcome = x }))
  <*> F.optionalFieldDef (makeFieldName "docs-outcome") (makeLens docsOutcome (\ r x -> r { docsOutcome = x })) NotTried
  <*> F.optionalFieldDef (makeFieldName "tests-outcome") (makeLens testsOutcome (\ r x -> r { testsOutcome = x })) NotTried

newtype Flags = Flags [(FlagName, Bool)]

instance Newtype.Newtype [(FlagName, Bool)] Flags

instance Text.Parsec Flags where
  parsec = undefined -- TODO

instance Text.Pretty Flags where
  pretty = undefined -- TODO

newtype Dependencies = Dependencies [PackageIdentifier]

instance Newtype.Newtype [PackageIdentifier] Dependencies

instance Text.Parsec Dependencies where
  parsec = undefined -- TODO

instance Text.Pretty Dependencies where
  pretty = undefined -- TODO

makeFieldName :: String -> F.FieldName
makeFieldName = B.pack

makeLens :: Functor f => (a -> s) -> (a -> t -> b) -> (s -> f t) -> a -> f b
makeLens getField setField f record = fmap (setField record) (f (getField record))

-- -----------------------------------------------------------------------------
-- Timestamps

-- | If the 'time' field is empty, fill it in with the current time.
affixTimestamp :: BuildReport -> IO BuildReport
affixTimestamp report = case time report of
    Nothing -> (\v -> report { time = Just v }) <$> getCurrentTime
    Just _ -> return report

-- -----------------------------------------------------------------------------
-- Parsing

read :: String -> BuildReport
read s = case parse s of
  Left  err -> error $ "error parsing build report: " ++ err
  Right rpt -> rpt

parse :: String -> Either String BuildReport
parse input
  = either (Left . Prelude.show) Right
  . snd
  . F.runParseResult
  $ F.parseFieldGrammar
    Spec.CabalSpecV1_10
    (undefined input) -- TODO: how do you make this conversion?
    buildReportFieldGrammar

-- -----------------------------------------------------------------------------
-- Pretty-printing

-- TODO: does this work?
show :: BuildReport -> String
show
  = F.showFields (const [])
  . F.prettyFieldGrammar Spec.CabalSpecV1_10 buildReportFieldGrammar

dispFlag :: (FlagName, Bool) -> Disp.Doc
dispFlag (fn, True)  =                       Disp.text (unFlagName fn)
dispFlag (fn, False) = Disp.char '-' Disp.<> Disp.text (unFlagName fn)

instance Text.Pretty InstallOutcome where
  pretty x = case x of
    PlanningFailed -> Disp.text "PlanningFailed"
    (DependencyFailed pkgid) -> Disp.text "DependencyFailed" <+> Text.pretty pkgid
    DownloadFailed -> Disp.text "DownloadFailed"
    UnpackFailed -> Disp.text "UnpackFailed"
    SetupFailed -> Disp.text "SetupFailed"
    ConfigureFailed -> Disp.text "ConfigureFailed"
    BuildFailed -> Disp.text "BuildFailed"
    InstallFailed -> Disp.text "InstallFailed"
    InstallOk -> Disp.text "InstallOk"

instance Text.Parsec InstallOutcome where
  parsec = do
    name <- Parse.munch1 Char.isAlphaNum
    case name of
      "PlanningFailed"   -> return PlanningFailed
      "DependencyFailed" -> do Parse.skipSpaces
                               pkgid <- Text.parsec
                               return (DependencyFailed pkgid)
      "DownloadFailed"   -> return DownloadFailed
      "UnpackFailed"     -> return UnpackFailed
      "SetupFailed"      -> return SetupFailed
      "ConfigureFailed"  -> return ConfigureFailed
      "BuildFailed"      -> return BuildFailed
      "InstallFailed"    -> return InstallFailed
      "InstallOk"        -> return InstallOk
      _                  -> Parse.pfail

instance Text.Pretty Outcome where
  pretty x = case x of
    NotTried -> Disp.text "NotTried"
    Failed -> Disp.text "Failed"
    Ok -> Disp.text "Ok"

instance Text.Parsec Outcome where
  parsec = do
    name <- Parse.munch1 Char.isAlpha
    case name of
      "NotTried" -> return NotTried
      "Failed"   -> return Failed
      "Ok"       -> return Ok
      _          -> Parse.pfail

instance MemSize BuildReport where
    memSize (BuildReport a b c d e f g h i j k l) = memSize10 a b c d e f g h i j + memSize k + memSize l

instance MemSize InstallOutcome where
    memSize (DependencyFailed a) = memSize1 a
    memSize _                    = memSize0

instance MemSize Outcome where
    memSize _ = memSize0

-------------------
-- HStringTemplate instances
--

instance ToSElem BuildReport where
    toSElem BuildReport{..} = SM . Map.fromList $
        [ ("package", display package)
        , ("time", toSElem time)
        , ("docBuilder", toSElem docBuilder)
        , ("os", display os)
        , ("arch", display arch)
        , ("compiler", display compiler)
        , ("client", display client)
        , ("flagAssignment", toSElem $ map (render . dispFlag) flagAssignment)
        , ("dependencies", toSElem $ map Text.display dependencies)
        , ("installOutcome", display installOutcome)
        , ("docsOutcome", display docsOutcome)
        , ("testsOutcome", display testsOutcome)
        ]
      where
        display value = toSElem (Text.display value)

-------------------
-- Arbitrary instances
--

instance Arbitrary BuildReport where
  arbitrary = BuildReport <$> arbitrary <*> arbitrary <*> arbitrary
                          <*> arbitrary <*> arbitrary <*> arbitrary
                          <*> arbitrary <*> arbitrary <*> arbitrary
                          <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary InstallOutcome where
  arbitrary = oneof [ pure PlanningFailed
                    , pure DependencyFailed <*> arbitrary
                    , pure DownloadFailed
                    , pure UnpackFailed
                    , pure SetupFailed
                    , pure ConfigureFailed
                    , pure BuildFailed
                    , pure InstallFailed
                    , pure InstallOk
                    ]

instance Arbitrary Outcome where
  arbitrary = elements [ NotTried, Failed, Ok ]


-------------------
-- SafeCopy instances
--

deriveSafeCopy 0 'base      ''Outcome
deriveSafeCopy 1 'extension ''InstallOutcome
deriveSafeCopy 3 'extension ''BuildReport


-------------------
-- Old SafeCopy versions
--

-- Note this is kind of backwards from a migration pov, but this is because
-- the oldest one used a textual external rep with a parser, and the only
-- version we have with a parser is the latest, but they're sufficiently
-- compatible that we can get away with it for now.
newtype BuildReport_v0 = BuildReport_v0 BuildReport

instance SafeCopy  BuildReport_v0
instance Serialize BuildReport_v0 where
    put (BuildReport_v0 br) = Serialize.put . BS.pack . show $ br
    get = (BuildReport_v0 . read . BS.unpack) `fmap` Serialize.get

instance Migrate BuildReport_v1 where
    type MigrateFrom BuildReport_v1 = BuildReport_v0
    migrate (BuildReport_v0 BuildReport{..}) = BuildReport_v1 {
        v1_package = package
      , v1_os = os
      , v1_arch = arch
      , v1_compiler = compiler
      , v1_client = client
      , v1_flagAssignment = flagAssignment
      , v1_dependencies = dependencies
      , v1_installOutcome = case installOutcome of
          PlanningFailed         -> error "impossible rev migration"
          DependencyFailed pkgid -> V0_DependencyFailed pkgid
          DownloadFailed         -> V0_DownloadFailed
          UnpackFailed           -> V0_UnpackFailed
          SetupFailed            -> V0_SetupFailed
          ConfigureFailed        -> V0_ConfigureFailed
          BuildFailed            -> V0_BuildFailed
          InstallFailed          -> V0_InstallFailed
          InstallOk              -> V0_InstallOk
      , v1_docsOutcome = docsOutcome
      , v1_testsOutcome = testsOutcome
      }

data BuildReport_v1 = BuildReport_v1 {
    v1_package         :: PackageIdentifier,
    v1_os              :: OS,
    v1_arch            :: Arch,
    v1_compiler        :: CompilerId,
    v1_client          :: PackageIdentifier,
    v1_flagAssignment  :: [(FlagName,Bool)],
    v1_dependencies    :: [PackageIdentifier],
    v1_installOutcome  :: InstallOutcome_v0,
    v1_docsOutcome     :: Outcome,
    v1_testsOutcome    :: Outcome
  }

data InstallOutcome_v0
   = V0_DependencyFailed PackageIdentifier
   | V0_DownloadFailed
   | V0_UnpackFailed
   | V0_SetupFailed
   | V0_ConfigureFailed
   | V0_BuildFailed
   | V0_InstallFailed
   | V0_InstallOk

deriveSafeCopy 0 'base      ''InstallOutcome_v0
deriveSafeCopy 2 'extension ''BuildReport_v1

instance Migrate BuildReport where
    type MigrateFrom BuildReport = BuildReport_v1
    migrate BuildReport_v1{..} = BuildReport {
        package = v1_package
      , time = Nothing
      , docBuilder = True  -- Most old reports come from the doc builder anyway
      , os = v1_os
      , arch = v1_arch
      , compiler = v1_compiler
      , client = v1_client
      , flagAssignment = v1_flagAssignment
      , dependencies = v1_dependencies
      , installOutcome = migrate v1_installOutcome
      , docsOutcome = v1_docsOutcome
      , testsOutcome = v1_testsOutcome
      }

instance Migrate InstallOutcome where
    type MigrateFrom InstallOutcome = InstallOutcome_v0
    migrate outcome = case outcome of
        V0_DependencyFailed pkgid -> DependencyFailed pkgid
        V0_DownloadFailed -> DownloadFailed
        V0_UnpackFailed -> UnpackFailed
        V0_SetupFailed -> SetupFailed
        V0_ConfigureFailed -> ConfigureFailed
        V0_BuildFailed -> BuildFailed
        V0_InstallFailed -> InstallFailed
        V0_InstallOk -> InstallOk
