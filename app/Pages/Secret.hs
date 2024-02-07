{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Pages.Secret (secretPage, SecretPage) where

import RIO
import Text.Blaze.Html5 as H
import Servant ((:>))
import qualified Servant as S
import qualified Servant.Auth.Server as SAS
import qualified Pages.Components as C

import Servant.HTML.Blaze(HTML)
import Types (User)

secretPageHtml :: User -> Html
secretPageHtml user = H.docTypeHtml $ do
    H.head $ do
      H.title "Secret Page"
    H.body $ do
      C.navbar "secret"
      H.h1 $ "TOP Secret!" <> H.toHtml user
      H.p "lorem ipsum"
      H.p "lorem ipsum"

secretPage :: HasLogFunc a => SAS.AuthResult User -> RIO a Html
secretPage (SAS.Authenticated user) = do
  logDebug $ "Hello from SecretPage " <> displayShow user

  pure $ secretPageHtml user
-- secretPage _ = S.throwError S.err401
secretPage _ =  logDebug "Hello from SecretPage Error" >> SAS.throwAll S.err404

type Protected = "secret" :> S.Get '[HTML] Html
type SecretPage auths = SAS.Auth auths User :> Protected
