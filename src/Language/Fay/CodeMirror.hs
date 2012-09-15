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
              -> Fay CodeMirror -- ^ A new code mirror.
newCodeMirror = ffi "CodeMirror(%1,{mode:%2,value:%3})"
