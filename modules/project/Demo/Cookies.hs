-- | Reading cookies.

module Demo.Cookies where

import Language.Fay.JQuery
import Language.Fay.JQuery.Cookie
import Language.Fay.Prelude
import Language.Fay.Print

main =
  ready $ do
    body <- select "body"
    group <- select "<div class='btn-group'></div>" >>= appendTo body
    select "<h3>Log:</h3>" >>= appendTo body

    select "<button class='btn'>Go Again</button>"
      >>= appendTo group
      >>= onClick (do readAgain; return False)

    select "<button class='btn'>Clear Cookie</button>"
      >>= appendTo group
      >>= onClick (do deleteCookie "demo"; return False)

    readAgain

readAgain = do
  cookie <- getCookie "demo"
  case cookie of
    Nothing -> do print "No cookie yet! Setting one."
                  setSessionCookie "demo" "Here's a cookie!"
    Just value -> print $ "The cookie value is: " ++ value
