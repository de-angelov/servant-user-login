{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
-- {-# LANGUAGE InstanceSigs #-}


module Types (
  AppConfig(..),
  HasServantPort,
  HasDbPool,
  User(..),
  HashedPassword(..),
  UserId(..),
  UserLoginData(..),
  UserPassword(..),
  dbPoolL,
  servantPortL,
  ) where
import RIO
import Data.Aeson (FromJSON, ToJSON)
import qualified Servant as S
import qualified Rel8 as R
import Control.Monad.Error.Class (MonadError)
import Text.Blaze.XHtml5 (ToMarkup (..))
import Servant.Auth.JWT (FromJWT)
import Servant.Auth.Server (ToJWT)
import Hasql.Pool (Pool)
import Text.Blaze.Html (Markup)

class HasDbPool env where
  dbPoolL :: Lens' env Pool

class HasServantPort env where
  servantPortL :: Lens' env Int

data AppConfig
  = AppConfig
  { appLogFunc :: !LogFunc
  , appPort :: !Int
  , appDBPool :: !Pool
  }

instance HasDbPool AppConfig where
  dbPoolL = lens appDBPool (\x y -> x { appDBPool = y })

instance MonadError S.ServerError (RIO a) where
  throwError = throwIO

instance HasLogFunc AppConfig where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })

instance HasServantPort AppConfig where
  servantPortL = lens appPort (\x y -> x { appPort = y })

newtype UserPassword
  = UserPassword Text
  deriving (Show, Eq, Generic,  FromJSON, ToJSON, ToJWT, ToMarkup)

newtype UserId = UserId {getUserId :: Int64}
  deriving stock (Generic)
  deriving newtype (Eq, Show, Read, FromJSON, ToJSON, R.DBEq, R.DBType)

newtype HashedPassword = HashedPassword Text
  deriving stock (Generic)
  deriving newtype (Eq, Show, Read, FromJSON, ToJSON, R.DBEq, R.DBType)


data UserLoginData
  = UserLoginData
  { username :: !Text
  , password :: !UserPassword
  } deriving (Show, Eq, Generic,  FromJSON, ToJSON)

data User
  = User
  { username :: !Text
  , id :: !Int64
  , password :: !HashedPassword
  } deriving (Show, Eq,  Generic, FromJSON, ToJSON, FromJWT, ToJWT)


instance ToMarkup User where
  toMarkup :: User -> Markup
  toMarkup = toMarkup . show


