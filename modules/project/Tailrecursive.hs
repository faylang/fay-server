-- | A demo to show that a tail-recursive function will not increase
-- the JavaScript stack.
--
-- See the ticket about optimizing tail-recursive functions for future
-- work <https://github.com/faylang/fay/issues/19>

{-# LANGUAGE NoImplicitPrelude #-}

module Tailrecursive where

import           Language.Fay.FFI
import           Language.Fay.Prelude

main = do
  benchmark
  benchmark
  benchmark
  benchmark

benchmark = do
  start <- getSeconds
  printD (sum 1000000 0 :: Double)
  end <- getSeconds
  printS (show (end-start) ++ "ms")

-- tail recursive
sum 0 acc = acc
sum n acc = sum (n - 1) (acc + n)

getSeconds :: Fay Double
getSeconds = ffi "new Date()"

printD :: Double -> Fay ()
printD = ffi "console.log(%1)"

printS :: String -> Fay ()
printS = ffi "console.log(%1)"
