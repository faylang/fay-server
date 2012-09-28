{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Fay.CodeMirror where

import Language.Fay.Prelude
import Language.Fay.FFI
import Language.Fay.DOM

data CodeMirror
instance Foreign CodeMirror

-- | Make a new code mirror.
newCodeMirror :: Element        -- ^ The parent element.
              -> String         -- ^ The mode.
              -> String         -- ^ The contents.
              -> Bool
              -> Fay CodeMirror -- ^ A new code mirror.
newCodeMirror = ffi "CodeMirror(%1,{mode:%2,value:%3,autofocus:%4,indentWithTabs:false,tabSize:4})"

-- | Get the current editor content.
getMirrorValue :: CodeMirror -> Fay String
getMirrorValue = ffi "%1['getValue']()"

-- | Set the current editor content.
setMirrorValue :: CodeMirror -> String -> Fay ()
setMirrorValue = ffi "%1['setValue'](%2)"

-- | Loosely capture live changes on an input and trigger a function for it.
setMirrorLiveChange :: CodeMirror -> Double -> Double -> Fay () -> Fay ()
setMirrorLiveChange = ffi "CodeMirrorLiveChange(%1,%2,%3,%4)"

-- | A CodeMirror line.
data CodeLine
instance Foreign CodeLine

-- | Set the classes for a line.
setMirrorLineClass :: CodeMirror -> Double -> String -> String -> Fay CodeLine
setMirrorLineClass = ffi "%1['setLineClass'](%2,%3,%4)"

-- | Set the classes for a line.
clearMirrorLineClass :: CodeMirror -> CodeLine -> Fay ()
clearMirrorLineClass = ffi "%2 && %1['setLineClass'](%2,null,null)"

-- | Set the classes for a line.
setReadOnly :: CodeMirror -> Bool -> Fay ()
setReadOnly = ffi "%1['setOption']('readOnly',%2)"

-- | Print using console.log.
send :: String -> Fay ()
send = ffi "window['console'] && window['console']['log'](\"%%s\",%1)"
