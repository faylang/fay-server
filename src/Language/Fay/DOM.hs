{-# LANGUAGE EmptyDataDecls #-}

module Language.Fay.DOM  where

import Language.Fay.FFI
import Language.Fay.Prelude

data Element
instance Foreign Element

-- | Get body.
getBody :: Fay Element
getBody = ffi "document['body']"