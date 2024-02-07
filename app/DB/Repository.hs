{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module DB.Repository (saveNewUser, getUser, getAllUsers) where

import RIO
import Types (User(..), UserId (..), HashedPassword (..) , UserPassword (..), UserLoginData(..), AppConfig, HasDbPool)
import qualified Crypto.KDF.BCrypt  as BCrypt
import Crypto.Random (getRandomBytes)
import Data.ByteArray (Bytes, convert)
import DB.Schema (userSchema, UserEntity (UserEntity, entityUserName), entityUserId, entityUserPassword, mapUserEntityToUser )
import qualified DB.Transaction as T
import qualified Rel8 as R
import Rel8 ((==.), (&&.))
import qualified Util


newtype Salt = Salt Bytes

newSalt :: MonadIO m => m Salt
newSalt = liftIO $ Salt <$> getRandomBytes 16

hashPasswordWithSalt :: UserPassword -> Salt -> HashedPassword
hashPasswordWithSalt (UserPassword password) (Salt salt) =
  let hash = BCrypt.bcrypt 10 salt (Util.fromTextToBytes password)
  in HashedPassword $ Util.fromBytesToTextUnsafe hash

hashPassword :: MonadIO m => UserPassword -> m HashedPassword
hashPassword password = hashPasswordWithSalt password <$> newSalt

insertUserStmt :: Text -> HashedPassword -> R.Insert [UserId]
insertUserStmt name hash =
  let
    rows = R.values
      [ UserEntity
          { entityUserId = R.unsafeCastExpr $ R.nextval  "users_user_id_seq"
          , entityUserName = R.lit name
          , entityUserPassword = R.lit hash
          }
      ]
  in
    R.Insert
    { R.into = userSchema
    , R.rows = rows
    , R.onConflict = R.DoNothing
    , R.returning = R.Projection entityUserId
    }


saveNewUser :: (HasDbPool env, HasLogFunc env) =>  Text -> UserPassword -> RIO env (Maybe User)
saveNewUser name pass = do
  hashedPassAndSalt <- liftIO $ hashPassword pass
  userIds <- T.executeStmt $ R.insert $ insertUserStmt name hashedPassAndSalt
  return $ listToMaybe userIds >>= \uid ->
    let UserId id = uid
    in  Just $ User
      { username = name
      , password = hashedPassAndSalt
      , id = id
      }


getUserStmt :: Text ->  R.Query (UserEntity R.Expr)
getUserStmt name = do
  user <- R.each userSchema
  R.where_ $ entityUserName user ==. R.lit name
  return user

verifyPassword :: UserPassword -> HashedPassword -> Bool
verifyPassword (UserPassword pass) (HashedPassword hash) =
  let
    passBytes = Util.fromTextToBytes pass
    hashBytes = Util.fromTextToBytes hash
  in BCrypt.validatePassword passBytes hashBytes

getUser :: (HasDbPool env, HasLogFunc env) =>  Text -> UserPassword -> RIO  env (Maybe User)
getUser uname pass  = do
  logDebug "saving new User"
  users <-   T.executeStmt $ R.select $ getUserStmt uname

  let verifyPassword' user =
            if verifyPassword pass (entityUserPassword user)
            then Just $ mapUserEntityToUser user
            else Nothing

  pure $ verifyPassword' =<< listToMaybe users
  -- return $ listToMaybe $  users

    -- = logDebug "saving new User"
    -- >> hashPassword pass
    -- >>= \hashedPass -> T.executeStmt $ R.select $ getUserStmt uname hashedPass
    -- >>= \users -> listToMaybe $ map  mapUserEntityToUser users

getAllUsersStmt :: R.Query (UserEntity R.Expr)
getAllUsersStmt = R.each userSchema


getAllUsers :: RIO AppConfig [User]
getAllUsers = do
  logDebug "getting all users"
  x <- T.executeStmt $ R.select getAllUsersStmt
  return $ map mapUserEntityToUser x
  -- return $ T.executeStmt $ R.select $ getAllUsersStmt ()


