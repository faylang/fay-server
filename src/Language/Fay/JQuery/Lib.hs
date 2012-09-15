{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Fay.JQuery.Lib where

import Language.Fay.Prelude
import Language.Fay.FFI
import Language.Fay.JQuery

onLiveChange :: Double -> Fay () -> JQuery -> Fay JQuery
onLiveChange = ffi "%3['livechange'](%1,%2)"
