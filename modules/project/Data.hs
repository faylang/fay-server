-- | Example of using `show' which is based on JSON.stringify. Don't
-- rely on this kind of serialization.

module Data where

import Language.Fay.FFI
import Language.Fay.Prelude

data Foo = Foo { x :: Double, y :: String, z :: Foo } | Bar
  deriving (Show)
instance Foreign Foo

main :: Fay ()
main = print (show (Foo 123 "abc" Bar))

print :: String -> Fay ()
print = ffi "console.log(%1)"
