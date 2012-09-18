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
import Language.Fay.Date
import Language.Fay.Prelude
import Language.Fay.ReactiveMvc

--------------------------------------------------------------------------------
-- Main.

-- | Main entry point.
main :: Fay ()
main = do
  ready $ do
    call ProjectModules $ \(ModuleList modules) -> do
      pm <- select "#project-modules"
      forM_ modules $ \m -> do
        li <- select "<li></li>"
        a <- select "<a href='#'></a>" & setText m & appendTo li
          & onClick (do chooseModule m; return True)
        after li pm
        return ()
      call LibraryModules $ \(ModuleList modules) -> do
        pm <- select "#library-modules"
        forM_ modules $ \m -> do
          li <- select "<li></li>"
          a <- select "<a href='#'></a>" & setText m & appendTo li
          after li pm
          return ()
        chooseModule "Alert"

chooseModule m =
  call (GetModule m) $ \result ->
    case result of
      NoModule name -> warn $ "No such module: " ++ name
      LoadedModule contents -> do
        bodyj <- select "#editor"
        empty bodyj
        body <- select "#editor" >>= getElement
        mirror <- newCodeMirror body "haskell" contents True
        lines_ref <- newRef []
        setMirrorLiveChange mirror (checkModule mirror lines_ref)
        checkModule mirror lines_ref
        select "#compile-btn" & onClick (do compileModule mirror; return False)
        return ()

compileModule :: CodeMirror -> Fay ()
compileModule mirror = do
  log "Compiling…"
  compileBtn <- select "#compile-btn"
  content <- getMirrorValue mirror
  call (CompileModule content) $ \result ->
    case result of
      CompileOk uid -> do
        setAttr "disabled" "disabled" compileBtn
        log $ "Compile OK: " ++ uid
        frame <- select "iframe"
        setAttr "src" ("/gen/" ++ uid ++ ".html") frame
        return ()
      CompileError output -> do
        log $ "Compile fail: " ++ output
        msgList <- select "#messages"
        empty msgList
        select "<div class='alert alert-error'>" & appendTo msgList & setText output
        return ()

-- | Check the current module.
checkModule :: CodeMirror -> Ref [CodeLine] -> Fay ()
checkModule mirror lines_ref = do
  compileBtn <- select "#compile-btn"
  setAttr "disabled" "disabled" compileBtn
  log "Checking module…"
  msgList <- select "#messages"
  setAttr "disabled" "disabled" compileBtn
  lines <- readRef lines_ref
  forM_ lines $ \line -> do
    clearMirrorLineClass mirror line
  writeRef lines_ref []
  content <- getMirrorValue mirror
  call (CheckModule content) $ \result -> do
    empty msgList
    case result of
      CheckOk uid -> do
        log $ "Check OK: " ++ uid
        removeAttr "disabled" compileBtn
        return ()
      CheckError msgs orig -> do
        log $ "Check failed: " ++ orig
        forM_ msgs $ \(Msg typ line err) -> do
          let classname = if typ == MsgWarning
                             then "compile-warning"
                             else "compile-error"
          mline <- setMirrorLineClass mirror (line-1) "" classname
          lines <- readRef lines_ref
          writeRef lines_ref (mline : lines)
          let classname = if typ == MsgWarning
                             then "<div class='alert alert-block'></span>"
                             else "<div class='alert alert-error'></span>"
          select classname & setText err & appendTo msgList

-- | Log something.
log :: String -> Fay ()
log text = do
  date <- getCurrentTime
  entry <- select "<div class='alert alert-info'></div>"
        & setText (formatDate "yy-MM-d HH:mm:ss" date ++ ": " ++ text)
  select "#log-tab" & prepend entry
  return ()

warn = log
