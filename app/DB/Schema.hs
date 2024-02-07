{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module DB.Schema where

import RIO
import qualified Rel8 as R
import Data.Aeson.Types (ToJSON,FromJSON)
import Types (UserId(..), HashedPassword(..), User(..))

data UserEntity f
  = UserEntity
  { entityUserId :: R.Column f UserId
  , entityUserName :: R.Column f Text
  , entityUserPassword :: R.Column f HashedPassword
  } deriving stock (Generic)
    deriving anyclass (R.Rel8able)


userSchema :: R.TableSchema (UserEntity R.Name)
userSchema
  = R.TableSchema
  { R.name = "users"
  , R.schema = Nothing
  , R.columns
    = UserEntity
    { entityUserId = "user_id"
    , entityUserName = "user_username"
    , entityUserPassword = "user_password"
    }
  }

mapUserEntityToUser :: UserEntity R.Result -> User
mapUserEntityToUser entity =
  let
    name =  entity.entityUserName
    pass =  entity.entityUserPassword
    UserId id = entity.entityUserId
  in
    User{ username = name, password = pass, id = id }
