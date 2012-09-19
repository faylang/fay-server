-- | A simple example of using the fundamental DOM operations.

{-# LANGUAGE EmptyDataDecls    #-}

module DOM where

import Language.Fay.FFI
import Language.Fay.Prelude
import Language.Fay.DOM

main :: Fay ()
main = addEventListener "load" printBody False

printBody :: Fay ()
printBody = do
  result <- documentGetElements "body"
  print result

print :: Foreign a => [a] -> Fay ()
print = ffi "console.log(%1)"

documentGetElements :: String -> Fay [Element]
documentGetElements = ffi "document.getElementsByTagName(%1)"

addEventListener :: String -> Fay () -> Bool -> Fay ()
addEventListener = ffi "window['addEventListener'](%1,%2,%3)"
