-- | Mutable references. Basically IORef implemented in JS.

module Ref where

import Language.Fay.FFI
import Language.Fay.Prelude
import Language.Fay.Ref

main :: Fay ()
main = do
  ref <- newRef "Hello, World!"
  x <- readRef ref
  print x
  writeRef ref "Hai!"
  readRef ref >>= print

print :: String -> Fay ()
print = ffi "console.log(%1)"
