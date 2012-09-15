{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -fno-warn-unused-do-bind -fno-warn-type-defaults -fno-warn-missing-signatures #-}

-- | The client.

module Client where

import Client.API
import SharedTypes

import Language.Fay.JQuery
import Language.Fay.JQuery.Lib
import Language.Fay.Prelude
import Language.Fay.FFI
import Language.Fay.ReactiveMvc

--------------------------------------------------------------------------------
-- Main.

-- | Main entry point.
main :: Fay ()
main = do
  ready $ do
    echo "Y"
    return ()
