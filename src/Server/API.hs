{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Server's API.

module Server.API where

import           Data.Aeson (encode,decode)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import           Language.Fay.Convert
import           Language.Fay.FFI (Foreign)
import           SharedTypes
import           Snap.Core

-- | There is one command dispatcher.
data Dispatcher = Dispatcher { unDispatcher :: Snap () }

-- | Reply with a command.
(<~) :: (Foreign a,Show a) => Returns a -> Snap a -> Dispatcher
_ <~ m = Dispatcher $ do
  x <- m
  writeLBS . encode . showToFay $ x

-- | Handle incoming JSON requests.
handle :: (Command -> Dispatcher) -> Snap ()
handle dispatch = do
  bytes <- getPostParam "json"
  case bytes of
    Nothing -> error $ "Invalid JSON: " ++ show bytes
    Just (L.fromChunks . return -> json) -> do
      case decode json >>= readFromFay of
        Nothing -> error $ "Unable to parse input: " ++ UTF8.toString (encode json)
        Just cmd -> do
          unDispatcher $ dispatch cmd
