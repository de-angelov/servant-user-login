{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module API.Auth where

import RIO
import Servant ((:>), (:<|>) (..))
import qualified Servant as S
import Types (User, UserLoginData(..), HasDbPool)
import qualified DB.Repository as DBR
import qualified Servant.Auth.Server as SAS

type Authentication
  = "api" :> "login"
  :> S.ReqBody '[S.JSON] UserLoginData
  :> S.Post '[S.JSON] User

type Registration
  = "api" :> "register"
  :> S.ReqBody '[S.JSON] UserLoginData
  :> S.Post '[S.JSON] User

type AuthAPI = Authentication :<|> Registration


login :: (HasDbPool env, HasLogFunc env) => UserLoginData -> RIO env User
login (UserLoginData name pass) = do
      uMaybe <- DBR.getUser name pass
      case uMaybe of
        Nothing -> logDebug "hello from failed login" >> SAS.throwAll S.err401
        Just u -> logDebug "hello from successful login" >> pure u

      -- let maybeUser = DBR.getUser user pass
      -- in maybeUser >>= traverse fromMaybe (SAS.throwAll S.err401)


register :: (HasDbPool env, HasLogFunc env) => UserLoginData -> RIO env User
register (UserLoginData user pass)= do
    logDebug "hello from  registration before anything happens "
    uMaybe <- DBR.saveNewUser user pass
    case uMaybe of
      Nothing -> logDebug "hello from failed registration" >> SAS.throwAll S.err422
      Just u -> logDebug "hello from successful registration" >> pure u

authAPI :: (HasDbPool env, HasLogFunc env) => S.ServerT AuthAPI (RIO env)
authAPI = login :<|> register
