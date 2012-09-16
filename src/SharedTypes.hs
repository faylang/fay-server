{-# OPTIONS -fno-warn-orphans #-}
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
  = CheckModule String (Returns CheckResult)
  | GetModule String (Returns Text)
  | CompileModule String (Returns CompileResult)
  deriving (Read,Data,Typeable,Show)
instance Foreign Command

-- | A check result.
data CheckResult
  = CheckOk String
  | CheckError [Msg] String
  deriving (Read,Data,Typeable,Show)
instance Foreign CheckResult
instance Record CheckResult

-- | A compile result.
data CompileResult
  = CompileOk String
  | CompileError String
  deriving (Read,Data,Typeable,Show)
instance Foreign CompileResult
instance Record CompileResult

-- | A msg.
data Msg
  = Msg MsgType Double String
  deriving (Read,Data,Typeable,Show)
instance Foreign Msg
instance Record Msg

-- | Message type.
data MsgType = MsgWarning | MsgError
  deriving (Read,Data,Typeable,Show,Eq)
instance Foreign MsgType

-- | Some text.
data Text = Text String
  deriving (Read,Data,Typeable,Show,Eq)
instance Foreign Text
instance Record Text

-- | A record type.
class Record a

-- | Foreign instance for Maybe.
instance Foreign a => Foreign (Maybe a)
