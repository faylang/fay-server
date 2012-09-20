{-# LANGUAGE NoImplicitPrelude #-}

module Demo.Timers where

import Language.Fay.DOM
import Language.Fay.Prelude
import Language.Fay.Print

main = do
  j <- setInterval 1000 $ print "JUMP!"
  p <- setInterval 800 $ print "Pogo pogo pogo pogo!"
  setTimeout 2000 $ do
    print "Actually, forget it."
    clearTimeout j
    clearTimeout p
  print "Here we go!"
