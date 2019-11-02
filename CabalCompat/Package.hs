module CabalCompat.Package
  ( module Distribution.Package
  , newPackageIdentifier
  , simpleParsePackageIdentifier
  ) where

import qualified CabalCompat.ReadP
import qualified Data.Char
import qualified Data.List
import qualified Data.Maybe
import Distribution.Package
import qualified Distribution.Types.Version

newPackageIdentifier :: PackageName -> [Int] -> PackageIdentifier
newPackageIdentifier name version = PackageIdentifier
  name
  (Distribution.Types.Version.mkVersion version)

simpleParsePackageIdentifier :: String -> Maybe (PackageName, [Int], [String])
simpleParsePackageIdentifier
  = fmap fst
  . Data.Maybe.listToMaybe
  . filter (all Data.Char.isSpace . snd)
  . CabalCompat.ReadP.readP_to_S parsePackageIdentifier

-- The Parsec instance for PackageIdentifier is broken in Cabal 2.4.1.0.
parsePackageIdentifier :: CabalCompat.ReadP.CabalParsing m => m (PackageName, [Int], [String])
parsePackageIdentifier = CabalCompat.ReadP.choice
  [ (,,)
    <$> parsePackageName
    <*> (CabalCompat.ReadP.char '-' *> parseVersionBranch)
    <*> CabalCompat.ReadP.option [] (CabalCompat.ReadP.char '-' *> parseVersionTags)
  , (,,)
    <$> parsePackageName
    <*> pure []
    <*> pure []
  ]

parsePackageName :: CabalCompat.ReadP.CabalParsing m => m PackageName
parsePackageName = fmap
  (mkPackageName . Data.List.intercalate "-")
  (CabalCompat.ReadP.sepBy1
    (do
      ys <- CabalCompat.ReadP.munch1 Data.Char.isAlphaNum
      if all Data.Char.isDigit ys
        then CabalCompat.ReadP.pfail
        else pure ys)
    (CabalCompat.ReadP.char '-'))

parseVersionBranch :: CabalCompat.ReadP.CabalParsing m => m [Int]
parseVersionBranch = CabalCompat.ReadP.sepBy1
  (fmap read (CabalCompat.ReadP.munch1 Data.Char.isDigit))
  (CabalCompat.ReadP.char '.')

parseVersionTags :: CabalCompat.ReadP.CabalParsing m => m [String]
parseVersionTags = CabalCompat.ReadP.sepBy1
  (CabalCompat.ReadP.munch1 Data.Char.isAlphaNum)
  (CabalCompat.ReadP.char '-')
