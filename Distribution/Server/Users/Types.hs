{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Distribution.Server.Users.Types (
    module Distribution.Server.Users.Types,
    module Distribution.Server.Users.AuthToken,
    module Distribution.Server.Framework.AuthTypes
  ) where

import Distribution.Server.Framework.AuthTypes
import Distribution.Server.Framework.MemSize
import Distribution.Server.Users.AuthToken

import CabalCompat.Text
         ( Text(..), Pretty(..), Parsec(..) )
import qualified Distribution.Server.Util.Parse as Parse
import qualified CabalCompat.ReadP as Parse
import qualified Text.PrettyPrint          as Disp
import qualified Data.Char as Char
import qualified Data.Text as T
import qualified Data.Map as M

import Control.Applicative ((<$>))
import Data.Aeson (ToJSON, FromJSON)
import Data.SafeCopy (base, extension, deriveSafeCopy, Migrate(..))
import Data.Typeable (Typeable)
import Data.Hashable


newtype UserId = UserId Int
  deriving (Eq, Ord, Read, Show, Typeable, MemSize, ToJSON, FromJSON)

newtype UserName  = UserName String
  deriving (Eq, Ord, Read, Show, Typeable, MemSize, ToJSON, FromJSON, Hashable)

data UserInfo = UserInfo {
                  userName   :: !UserName,
                  userStatus :: !UserStatus,
                  userTokens :: !(M.Map AuthToken T.Text) -- tokens and descriptions
                } deriving (Eq, Show, Typeable)

data UserStatus = AccountEnabled  UserAuth
                | AccountDisabled (Maybe UserAuth)
                | AccountDeleted
    deriving (Eq, Show, Typeable)

newtype UserAuth = UserAuth PasswdHash
    deriving (Show, Eq, Typeable)

isActiveAccount :: UserStatus -> Bool
isActiveAccount (AccountEnabled  _) = True
isActiveAccount (AccountDisabled _) = True
isActiveAccount  AccountDeleted     = False

instance MemSize UserInfo where
    memSize (UserInfo a b c) = memSize3 a b c

instance MemSize UserStatus where
    memSize (AccountEnabled  a) = memSize1 a
    memSize (AccountDisabled a) = memSize1 a
    memSize (AccountDeleted)    = memSize0

instance MemSize UserAuth where
    memSize (UserAuth a) = memSize1 a

instance Pretty UserId where
  pretty (UserId uid) = Disp.int uid

instance Parsec UserId where
  parsec = UserId <$> Parse.int

instance Text UserId where
    disp = pretty
    parse = parsec

instance Pretty UserName where
  pretty (UserName name) = Disp.text name

instance Parsec UserName where
  parsec = UserName <$> Parse.munch1 isValidUserNameChar

instance Text UserName where
    disp = pretty
    parse = parsec

isValidUserNameChar :: Char -> Bool
isValidUserNameChar c = (c < '\127' && Char.isAlphaNum c) || (c == '_')

data UserInfo_v0 = UserInfo_v0 {
                  userName_v0   :: !UserName,
                  userStatus_v0 :: !UserStatus
                } deriving (Eq, Show, Typeable)

instance Migrate UserInfo where
    type MigrateFrom UserInfo = UserInfo_v0
    migrate v0 =
        UserInfo
        { userName = userName_v0 v0
        , userStatus = userStatus_v0 v0
        , userTokens = M.empty
        }

$(deriveSafeCopy 0 'base ''UserId)
$(deriveSafeCopy 0 'base ''UserName)
$(deriveSafeCopy 1 'base ''UserAuth)
$(deriveSafeCopy 0 'base ''UserStatus)
$(deriveSafeCopy 0 'base ''UserInfo_v0)
$(deriveSafeCopy 1 'extension ''UserInfo)
