{-# LANGUAGE NoImplicitPrelude #-}

module Timers where

import Language.Fay.DOM
import Language.Fay.Prelude
import Language.Fay.Print

main = do
  setInterval 1000 $ print "Pogo pogo pogo pogo!"
  print "Here we go!"
