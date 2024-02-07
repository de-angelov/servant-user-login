module Pages.Components (navbar) where 

import RIO
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html5((!))

li :: Text -> Text -> Bool -> H.Html
li href text isActive  = 
  let 
    className = if isActive then "active" else ""
  in 
    H.li
    $ H.a
      ! A.href (H.toValue href) 
      ! A.class_ className 
      $ H.toHtml text
  
navbar :: Text -> H.Html
navbar active =  
  let
    items = [ ("home", "/"),("login", "/login"), ("secret", "/secret")]
    getElements = mapM_ (\(text,href) -> li href text (text == active)) 
  in H.nav $ getElements items