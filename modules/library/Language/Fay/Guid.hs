{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Globally unique ids.

module Language.Fay.Guid where

import Language.Fay.FFI
import Language.Fay.Prelude

data Guid
instance Eq Guid
instance Foreign Guid

-- | Make a new globally unique id.
newGuid :: Fay Guid
newGuid = ffi "{}"
