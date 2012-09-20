-- | A trivial example of using JQuery.

module Demo.JQuery where

import Language.Fay.Date
import Language.Fay.FFI
import Language.Fay.JQuery
import Language.Fay.Prelude

main :: Fay ()
main =
  ready $ void $ do
    body <- select "body"
    p <- select "<p></p>" >>= appendTo body
    select "<a class='btn'></a>"
      >>= appendTo p
      >>= setText "Click me!"
      >>= onClick (do now <- getCurrentTime
                      alert (toISOString now ++ ": Clicktastic!")
                      return False)

-- | Alert using window.alert, this is a trivial example of the FFI.
alert :: String -> Fay ()
alert = ffi "window.alert(%1)"
