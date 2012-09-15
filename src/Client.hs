{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -fno-warn-unused-do-bind -fno-warn-type-defaults -fno-warn-missing-signatures #-}

-- | The client.

module Client where

import Language.Fay.CodeMirror
import Language.Fay.DOM
import Language.Fay.JQuery
import Language.Fay.Prelude
import Language.Fay.ReactiveMvc

--------------------------------------------------------------------------------
-- Main.

-- | Main entry point.
main :: Fay ()
main = do
  ready $ do
    body <- select "body"
    compileBtn <- newButton "Compile" & appendTo body
    loadBtn <- newButton "Run" & appendTo body & setAttr "disabled" "disabled"
    body <- getBody
    editor <- newCodeMirror body "haskell" "module Hello (foo) where\n\nmain = print 'a'"
    nav <- select "#file-tabs-nav"
    li <- select "<li><a data-toggle='tab' href='#start'>start.hs</a></li>" & appendTo nav
    navc <- select "#file-tabs-content"
    li <- select "<div class='tab-pane' id='start'>start.hs content</div>" & appendTo navc
    return ()

newButton title = select "<a class='btn'></a>" & setHtml title
