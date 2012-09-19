-- | A trivial Hello, World! example.

module Alert where

import Language.Fay.FFI
import Language.Fay.Prelude

main :: Fay ()
main = alert "Hello, World!"

-- | Alert using window.alert, this is a trivial example of the FFI.
alert :: String -> Fay ()
alert = ffi "window.alert(%1)"
