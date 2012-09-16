{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE EmptyDataDecls #-}

module Language.Fay.DOM  where

import Language.Fay.FFI
import Language.Fay.Prelude

data Element
instance Foreign Element

-- | Get body.
getBody :: Fay Element
getBody = ffi "document['body']"

data Timer
instance Foreign Timer
instance Foreign (Maybe Timer)

-- | Set a timer.
setTimeout :: Fay () -> Double -> Fay Timer
setTimeout = ffi "window['setTimeout'](%1,%2)"

-- | Clear a timer.
clearTimeout :: Timer -> Fay ()
clearTimeout = ffi "window['clearTimeout'](%1)"
