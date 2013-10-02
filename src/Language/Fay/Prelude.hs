{-# LANGUAGE CPP #-}
module Language.Fay.Prelude (Foreign, module Prelude, Typeable, Data, forM_, when, void, unless) where

import Data.Data
import Language.Fay.FFI (Foreign)
import Prelude
#ifndef FAY
import Control.Monad (forM_, when, void, unless)
#endif
