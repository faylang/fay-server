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

-- | Set a timer.
setTimeout :: Double -> Fay () -> Fay Timer
setTimeout = ffi "window['setTimeout'](%2,%1)"

-- | Set a timer.
setInterval :: Double -> Fay () -> Fay Timer
setInterval = ffi "window['setInterval'](%2,%1)"

-- | Clear a timer.
clearTimeout :: Timer -> Fay ()
clearTimeout = ffi "window['clearTimeout'](%1)"

alert :: String -> Fay ()
alert = ffi "window.alert(%1)"
