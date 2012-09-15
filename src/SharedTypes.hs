{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Data types shared between client and server.

module SharedTypes where

import Language.Fay.FFI
import Language.Fay.Prelude

-- | A phantom type which ensures the connection between the command
-- and the return value.
data Returns a = Returns
  deriving (Read,Data,Typeable,Show)

-- | The command list.
data Command
  = SaveFile String String (Returns ())
  | ReadFile String (Returns String)
  | CheckFile String (Returns (Maybe String))
  deriving (Read,Data,Typeable,Show)
instance Foreign Command

-- | A record type.
class Record a

-- | Foreign instance for Maybe.
instance Foreign a => Foreign (Maybe a)