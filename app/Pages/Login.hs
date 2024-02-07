{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Pages.Login (loginPage, LoginPage) where

import RIO
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Servant ((:>))
import qualified Servant as S
import Servant.HTML.Blaze(HTML)
import qualified Pages.Components as C

loginPageHtml :: Html
loginPageHtml =  H.docTypeHtml $ do
  H.head $ do
    H.title "Login Page"
  H.body $ do
    C.navbar "login"
    H.h1 "Its Login Time!"
    H.form
      ! A.id "form"
      $ do
        H.input
          ! A.id "username"
          ! A.type_  "text"
          ! A.name "username"
        H.input
          ! A.id "password"
          ! A.type_ "text"
          ! A.id "user-input"
          ! A.name "password"
        H.input
          ! A.type_ "submit"
          ! A.value "register"
          ! A.onclick "submitToAPI('/api/register')"
        H.input
          ! A.type_ "submit"

          ! A.value "log in"
          ! A.onclick "submitToAPI('/api/login')"
    H.script
      ! A.type_ "text/javascript"
      $
        "var form = document.getElementById('form');"
        <> "function handleForm(event) { event.preventDefault(); }"
        <> "form.addEventListener('submit', handleForm);"
        <> "function submitToAPI(apiUrl) { "
        <> "console.log('api =>', apiUrl);"
        <> "var password = document.getElementById('password')?.value;"
        <> "var username = document.getElementById('username')?.value;"
        <> "fetch(apiUrl,"
        <> "{method: 'POST',"
        <> "body: JSON.stringify({ username, password }),"
        <> "headers: new Headers({'content-type': 'application/json'}),"
        <> "}).then(response => {"
        <> "console.log('response after then', response);"
        <> " return response.json();"
        <> "}).then(data => {"
        <> "console.log('data', data);"
        <> "alert(data);"
        <> "}).catch(err => {"
        <> "console.log('error', err);"
        <> "});"
        <> "}"




loginPage :: HasLogFunc env => S.ServerT LoginPage (RIO env)
loginPage =  logDebug "Login Page Request" >> pure loginPageHtml

type LoginPage = "login" :> S.Get '[HTML] Html
