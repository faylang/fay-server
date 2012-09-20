{-# LANGUAGE NoImplicitPrelude #-}

module Language.Fay.Print where

import Language.Fay.Prelude
import Language.Fay.FFI

print :: String -> Fay ()
print = ffi "console.log(%1)"

alert :: String -> Fay ()
alert = ffi "window.alert(%1)"
