-- | A demo to show that a tail-recursive function will not increase
-- the JavaScript stack.
--
-- More future work for TCO to be done here.

module Demo.Tailrecursive where

import Language.Fay.FFI
import Language.Fay.Prelude

main :: Fay ()
main = do
  benchmark
  benchmark
  benchmark
  benchmark

benchmark :: Fay ()
benchmark = do
  start <- getSeconds
  printD (sum 1000000 0 :: Double)
  end <- getSeconds
  printS (show (end-start) ++ "ms")

-- the tail recursive function
sum :: Double -> Double -> Double
sum 0 acc = acc
sum n acc = sum (n - 1) (acc + n)

getSeconds :: Fay Double
getSeconds = ffi "new Date()"

printD :: Double -> Fay ()
printD = ffi "console.log(%1)"

printS :: String -> Fay ()
printS = ffi "console.log(%1)"
