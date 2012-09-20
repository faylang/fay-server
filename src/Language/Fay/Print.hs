{-# LANGUAGE NoImplicitPrelude #-}

module Language.Fay.Print where

import Language.Fay.FFI
import Language.Fay.Prelude

-- | Print using console.log.
echo :: String -> Fay ()
echo = ffi "window['console'] && window['console']['log'](\"%%s\",%1)"

-- | Print using console.log.
print :: Foreign a => a -> Fay ()
print = ffi "window['console'] && window['console']['log'](\"%%o\",%1)"
