{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}

module Pages.Secret (secretPage, SecretPage) where

import qualified Pages.Components as C
import RIO
import Servant ((:>))
import qualified Servant as S
import qualified Servant.Auth.Server as SAS
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Servant.HTML.Blaze (HTML)
import Types (User)
import Text.Heredoc ( here )

js :: Text
js =
  [here|
    function logoff() {
    fetch("/api/login",
      {
        method: 'DELETE',
        headers: new Headers({'content-type': 'application/json'}),
      })
    .then(_ => window.location.href = '/login')
    .catch(err => {
      console.log('error', err);
    });
    }
  |]

secretPageHtml :: User -> Html
secretPageHtml user = H.docTypeHtml $ do
  H.head $ do
    H.title "Secret Page"
  H.body $ do
    C.navbar "secret"
    H.h1 $ "TOP SECRET: " <> H.toHtml user
    H.br
    H.button
        ! A.onclick "logoff()"
        $ "log off"
    H.script
      ! A.type_ "text/javascript"
      $ toMarkup js

secretPage :: (HasLogFunc a) => SAS.AuthResult User -> RIO a Html
secretPage (SAS.Authenticated user) = do
  logDebug $ "Hello from SecretPage " <> displayShow user

  pure $ secretPageHtml user
-- secretPage _ = S.throwError S.err401
-- secretPage _ =  logDebug "Hello from SecretPage Error" >> SAS.throwAll S.err401
secretPage SAS.BadPassword = logDebug "Hello from SecretPage BadPassword Error" >> SAS.throwAll S.err401
secretPage SAS.NoSuchUser = logDebug "Hello from SecretPage NoSuchUser Error" >> SAS.throwAll S.err401
secretPage SAS.Indefinite = logDebug "Hello from SecretPage Indefinite Error" >> SAS.throwAll S.err401

type Protected = "secret" :> S.Get '[HTML] Html
type SecretPage auths = SAS.Auth auths User :> Protected
