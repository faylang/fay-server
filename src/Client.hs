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

import Language.Fay.CodeMirror
import Language.Fay.JQuery
import Language.Fay.Ref
import Language.Fay.Prelude
import Language.Fay.ReactiveMvc

--------------------------------------------------------------------------------
-- Main.

-- | Main entry point.
main :: Fay ()
main = do
  ready $ do
    call (GetModule "demos/demo.hs") $ \(Text contents) -> do
      bodyj <- select "#editor"
      body <- select "#editor" >>= getElement
      mirror <- newCodeMirror body "haskell" contents True
      msgList <- select "#messages"
      lines_ref <- newRef []
      let compileModule = do
            lines <- readRef lines_ref
            forM_ lines $ \line -> do
              clearMirrorLineClass mirror line
            writeRef lines_ref []
            content <- getMirrorValue mirror
            call (CheckModule content) $ \result -> do
              empty msgList
              case result of
                CheckOk -> return ()
                CheckError msgs orig -> do
                  forM_ msgs $ \(Msg typ line err) -> do
                    let classname = if typ == MsgWarning
                                       then "compile-warning"
                                       else "compile-error"
                    mline <- setMirrorLineClass mirror (line-1) "" classname
                    lines <- readRef lines_ref
                    writeRef lines_ref (mline : lines)
                    if typ == MsgWarning
                       then select "<div class='alert alert-block'>*</span>" & setText err & appendTo msgList
                       else select "<div class='alert alert-error'>*</span>" & setText err & appendTo msgList
      setMirrorLiveChange mirror 500 compileModule
      compileModule
