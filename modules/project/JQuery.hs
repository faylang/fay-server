{-# LANGUAGE NoImplicitPrelude #-}

module JQuery where

import Language.Fay.JQuery
import Language.Fay.Prelude

main :: Fay ()
main =
  ready $ void $ do
    body <- select "body"
    setText "Hello, World!" body
