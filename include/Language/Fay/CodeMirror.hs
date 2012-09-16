{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Fay.CodeMirror where

import Language.Fay.Prelude
import Language.Fay.FFI
import Language.Fay.DOM
import Language.Fay.Ref

data CodeMirror
instance Foreign CodeMirror

-- | Make a new code mirror.
newCodeMirror :: Element        -- ^ The parent element.
              -> String         -- ^ The mode.
              -> String         -- ^ The contents.
              -> Bool
              -> Fay CodeMirror -- ^ A new code mirror.
newCodeMirror = ffi "CodeMirror(%1,{mode:%2,value:%3,autofocus:%4,indentWithTabs:true})"

-- | Hook for changes in the editor.
setMirrorOnChange :: CodeMirror -> Fay () -> Fay ()
setMirrorOnChange = ffi "%1.setOption('onChange',%2,200)"

-- | Get the current editor content.
getMirrorValue :: CodeMirror -> Fay String
getMirrorValue = ffi "%1.getValue()"

-- | Loosely capture live changes on an input and trigger a function for it.
setMirrorLiveChange :: CodeMirror -> Double -> Fay () -> Fay ()
setMirrorLiveChange mirror delay func = do
  timeout_ref <- newRef Nothing
  setMirrorOnChange mirror $ do
    mt <- readRef timeout_ref
    case mt of
      Just timeout -> clearTimeout timeout
      Nothing      -> return ()
    newtimeout <- setTimeout func delay
    writeRef timeout_ref (Just newtimeout)

-- | A CodeMirror line.
data CodeLine
instance Foreign CodeLine

-- | Set the classes for a line.
setMirrorLineClass :: CodeMirror -> Double -> String -> String -> Fay CodeLine
setMirrorLineClass = ffi "%1.setLineClass(%2,%3,%4)"

-- | Set the classes for a line.
clearMirrorLineClass :: CodeMirror -> CodeLine -> Fay ()
clearMirrorLineClass = ffi "%2 && %1.setLineClass(%2,null,null)"
