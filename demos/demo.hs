{-# LANGUAGE NoImplicitPrelude #-}

module Start where

import Language.Fay.Prelude
import Language.Fay.JQuery

main :: Fay ()
main = do
  ready $ do
    body <- select "body"
    setText "Hello, World!" body
    return ()
