{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The server.

module Server where

import Server.API
import SharedTypes

import Data.Char
import Data.List
import Data.Ord
import Snap.Core
import Snap.Http.Server

-- | Main entry point.
server :: IO ()
server = do
  httpServe (setPort 10001 defaultConfig) (route [("/json",handle dispatcher)])

-- | Dispatch on the commands.
dispatcher :: Command -> Dispatcher
dispatcher cmd =
  case cmd of
    SaveFile path contents r -> r <~ do
      return ()
    ReadFile path r -> r <~ do
      return "module Demo where"
    CheckFile path r -> r <~ do
      return (Just "src/X.hs:2:5: Not in scope `example'")
