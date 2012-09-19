-- | Another trivial example, using console.

{-# LANGUAGE NoImplicitPrelude #-}

module Console where

import Language.Fay.FFI
import Language.Fay.Prelude

main :: Fay ()
main = do
  print "Hello, World!"
  printAnything "Beep!"

-- | The argument must be monomorphic, otherwise Fay can't tell how to serialize it.
print :: String -> Fay ()
print = ffi "console.log(%1)"

-- | If it's polymorphic, Fay won't do anything to it.
printAnything :: Foreign a => a -> Fay ()
printAnything = ffi "console.log(JSON.stringify(%1))"
