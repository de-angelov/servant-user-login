{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Replace case with maybe" #-}

module API.Auth where

import RIO
import Servant ((:>), (:<|>) (..))
import qualified Servant as S
import Types (User, UserLoginData(..), HasDbPool)
import qualified DB.Repository as DBR
import qualified Servant.Auth.Server as SAS


type AuthHeaders = S.Headers  '[ S.Header "Set-Cookie" SAS.SetCookie, S.Header "Set-Cookie" SAS.SetCookie]
type Login
  = "api" :> "login"
  :> S.ReqBody '[S.JSON] UserLoginData
  :> S.Post '[S.JSON] (AuthHeaders User)
  -- :> S.Verb 'S.POST 204 '[S.JSON] (AuthHeaders User)
  -- :> S.Post '[S.JSON] (S.Headers '[S.Header "Set-Cookie" SAS.SetCookie, S.Header "Set-Cookie" SAS.SetCookie], User )

type Logout
  = "api" :> "login"
  :> S.Delete '[S.JSON] (AuthHeaders S.NoContent)


type Register
  = "api" :> "register"
  :> S.ReqBody '[S.JSON] UserLoginData
  :> S.Post '[S.JSON] (AuthHeaders User)



type AuthAPI =  Login :<|> Logout :<|> Register

logout
  :: (HasLogFunc env)
  => SAS.CookieSettings
  -> RIO env (AuthHeaders S.NoContent)
logout cookieSettings = do
  logInfo "Hello from User Log Out"
  pure $ SAS.clearSession cookieSettings S.NoContent

login :: (HasDbPool env, HasLogFunc env)
      => SAS.CookieSettings
      -> SAS.JWTSettings
      -> UserLoginData
      -> RIO env (AuthHeaders User)
login cookieSettings jwtSettings (UserLoginData name pass) =
    let
      handleFailedLogin =  logDebug "hello from failed login" >> SAS.throwAll S.err401

      handleSuccessfulLogin (u :: User) = do
          logDebug "hello from successful login"
          liftIO $ SAS.acceptLogin cookieSettings jwtSettings u
          >>= \case
              Nothing -> SAS.throwAll S.err401
              Just applyCookies  -> pure  $ applyCookies u
    in
      DBR.getUser name pass >>= maybe handleFailedLogin handleSuccessfulLogin

register :: (HasDbPool env, HasLogFunc env)
      => SAS.CookieSettings
      -> SAS.JWTSettings
      -> UserLoginData
      -> RIO env (AuthHeaders User)
register cookieSettings jwtSettings (UserLoginData user pass)= do
    logDebug "hello from  Register before anything happens "
    uMaybe <- DBR.saveNewUser user pass
    case uMaybe of
      Nothing -> handleFailedRegister
      Just u -> handleSuccessfulRegister u

    where
      handleFailedRegister =
        logDebug "hello from failed Register"
        >> SAS.throwAll S.err422

      handleSuccessfulRegister u = do
        logDebug "hello from successful Register"
        liftIO $ SAS.acceptLogin cookieSettings jwtSettings u
        >>= \case
              Nothing ->  handleFailedRegister
              Just applyCookies -> pure $ applyCookies u


authAPI
  :: (HasDbPool env, HasLogFunc env)
  => SAS.CookieSettings
  -> SAS.JWTSettings
  -> S.ServerT AuthAPI (RIO env)
authAPI cs jwts = login cs jwts :<|> logout cs :<|> register cs jwts
