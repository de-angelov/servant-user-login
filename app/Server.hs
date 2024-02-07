{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Server (startServer) where

import RIO
import Servant ((:<|>)(..), (:>), Context ((:.)) )
import Types (User, HasDbPool)
import qualified Servant as S



import Types (AppConfig (appPort))
import Pages.Home (HomePage, homePage)
import Pages.Login (LoginPage, loginPage)
import Pages.Secret (SecretPage, secretPage)
import API.Auth (AuthAPI, authAPI)
import Control.Monad.Except (ExceptT(ExceptT))
import Network.Wai(Middleware)
import qualified Network.Wai.Handler.Warp as Warp
import qualified Servant.Auth.Server as SAS
import qualified Network.Wai.Middleware.RequestLogger as RL
import qualified Network.Wai.Middleware.RequestLogger.JSON as RLJ

type ServerAPI auths
  = HomePage
  :<|> LoginPage
  :<|> SecretPage auths
  :<|> AuthAPI

server :: (HasDbPool a, HasLogFunc a) => S.ServerT (ServerAPI auths) (RIO a)
server
  = homePage
  :<|> loginPage
  :<|> secretPage
  :<|> authAPI

apiProxy :: S.Proxy (ServerAPI '[SAS.Cookie])
apiProxy = S.Proxy


rioToHandler :: a -> RIO a b -> S.Handler b
rioToHandler env app = S.Handler $ ExceptT $ try $ runRIO env app


mkApp :: S.Context '[SAS.CookieSettings, SAS.JWTSettings] -> AppConfig -> S.Application
mkApp ctx config
  = S.serveWithContext apiProxy ctx rioServer
  where
    rioServer = S.hoistServerWithContext
                      apiProxy
                      (S.Proxy :: S.Proxy '[SAS.CookieSettings, SAS.JWTSettings])
                      (rioToHandler config) server


jsonRequestLogger :: RIO AppConfig Middleware
jsonRequestLogger = liftIO $ RL.mkRequestLogger $ SAS.def { RL.outputFormat = RL.CustomOutputFormatWithDetails RLJ.formatAsJSON }

startServer :: AppConfig -> RIO AppConfig ()
startServer config = do
  key <- liftIO SAS.generateKey
  warpLogger <- jsonRequestLogger

  let
    port = appPort config
    ctx
      = SAS.defaultCookieSettings
      :. SAS.defaultJWTSettings key
      :. S.EmptyContext
    app = mkApp ctx config

    settings = Warp.defaultSettings
      & Warp.setPort port
      & Warp.setTimeout 60


  logDebug $ "Starting Server inside inner RIO on port " <> displayShow port

  liftIO $  Warp.runSettings settings $ warpLogger app




  -- >>= \warpLogger  -> liftIO undefined

-- startServer :: AppConfig -> RIO AppConfig ()
-- startServer config
--   = liftIO SAS.generateKey
--   >>= \key ->
--   let
--     port = appPort config
--     ctx
--       = SAS.defaultCookieSettings
--       :. SAS.defaultJWTSettings key
--       :. S.EmptyContext
--     app = mkApp ctx config

--     settings = Warp.defaultSettings
--       & Warp.setPort port
--       & Warp.setTimeout 60

--     message = "Starting Server inside inner RIO on port " <> displayShow port

--     jsonRequestLogger = RL.mkRequestLogger $ SAS.def { RL.outputFormat = RL.CustomOutputFormatWithDetails RLJ.formatAsJSON }

--   in logDebug message
--   >> jsonRequestLogger
--   >>= \warpLogger ->  liftIO (Warp.runSettings $ warpLogger $ settings app)
--   -- >>= \warpLogger  -> liftIO undefined

