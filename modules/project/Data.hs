-- Generate Show value.

{-# LANGUAGE NoImplicitPrelude #-}

module Data where

import Language.Fay.FFI
import Language.Fay.Prelude

data Foo = Foo { x :: Double, y :: String, z :: Foo } | Bar
  deriving (Show)
instance Foreign Foo

main = print (show (Foo 123 "abc" Bar))

print :: String -> Fay ()
print = ffi "console.log(%1)"
