{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -fno-warn-unused-do-bind -fno-warn-type-defaults -fno-warn-missing-signatures #-}
{-# OPTIONS -fno-warn-name-shadowing #-}

-- | The client.

module Client (main) where

import Client.API
import SharedTypes

import Language.Fay.CodeMirror
import Language.Fay.JQuery
import Language.Fay.Ref
import Language.Fay.DOM
import Language.Fay.Date
import Language.Fay.Prelude
import Language.Fay.ReactiveMvc
import Language.Fay.FFI

--------------------------------------------------------------------------------
-- Main.

-- | Main entry point.
main :: Fay ()
main =
  ready $ void $ do
    mirror <- makeMirror
    loadModules mirror LibraryModules
    loadModules mirror ProjectModules
    loadModules mirror InternalModules
    loadModules mirror GlobalModules
    newModule

newModule = do

  input <- select "#new-module-name"
  btn <- select "#new-module-btn"
  validateMsg <- select "#module-validate"

  let update = do
        val <- getVal input
        unless (val == "") $
          call (CleanModuleName val) $ \result -> do
            handleValidateResult result

      make = true $ do
        val <- select "#new-module-name" & getVal
        call (CleanModuleName val) $ \result -> do
          case result of
            CleanModule name ->
              call (CreateModule name) $ \result -> do
                handleValidateResult result
                case result of
                  CleanModule _ -> select "#new-module" & hideModal
                  _ -> return ()
            _ -> return ()

      handleValidateResult result =
        case result of
          CleanModule name -> do
            setVal name input
            removeAttr "disabled" btn
            hide validateMsg
            return ()
          InvalidModule err -> do
            setAttr "disabled" "disabled" btn
            setText err validateMsg & unhide
            return ()

  select "#new-module-name" & onLiveChange 100 update
  select "#new-module-btn" & onClick make

makeMirror = do
  bodyj <- select "#editor"
  empty bodyj
  body <- select "#editor" >>= getElement
  mirror <- newCodeMirror body "haskell" "-- Here we go…" True
  lines_ref <- newRef []
  setMirrorLiveChange mirror 500 200 (checkModule mirror lines_ref)
  select "#compile-btn" & onClick (do compileModule mirror; return False)
  return mirror

loadModules mirror typ =
  call typ $ \(ModuleList modules) -> do
    navitor <- select "#navitor"
    pm <- select (case typ Returns of
                   ProjectModules _ -> "#project-modules"
                   LibraryModules _ -> "#library-modules"
                   InternalModules _ -> "#internal-modules"
                   GlobalModules _ -> "#global-modules"
                   _ -> "")
    forM_ (reverse (zip [0..] modules)) $ \(i,m) -> do
      li <- select "<li></li>"
      select "<a href='#'></a>" & setText m & appendTo li
        & onClick (do findSelector "li" navitor & removeClass "active"
                      addClass "active" li
                      chooseModule (typ Returns) mirror m
                      return True)
      when (i == 0) $
        case typ Returns of
          ProjectModules _ -> void $ addClass "active" li
          _ -> return ()
      after li pm
      return ()
    case typ Returns of
      ProjectModules _ ->
        case modules of
          (x:_) -> chooseModule (typ Returns) mirror x
          _      -> return ()
      _ -> return ()

chooseModule typ mirror m =
  call (GetModule m) $ \result ->
    case result of
      NoModule name -> warn $ "No such module: " ++ name
      LoadedModule contents -> do
        let compileSupport = do
              select "#compile-btn" & unhide
              select "#compile-status" & unhide
              setReadOnly mirror False
        case typ of
          ProjectModules _ -> compileSupport
          GlobalModules _ -> compileSupport
          _ -> do
            select "#compile-btn" & hide
            select "#compile-status" & hide
            setReadOnly mirror True
        setMirrorValue mirror contents

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
        select "#compile-status" & setText "Loaded" & setAttr "class" "label label-info"
        return ()
      CompileError output -> do
        log $ "Compile fail: " ++ output
        msgList <- select "#messages"
        empty msgList
        select "<div class='alert alert-error'>" & appendTo msgList & setText output
        select "#compile-status" & setText "Problem" & setAttr "class" "label label-important"
        return ()

-- | Check the current module.
checkModule :: CodeMirror -> Ref [CodeLine] -> Fay ()
checkModule mirror lines_ref = do
  compileBtn <- select "#compile-btn"
  setAttr "disabled" "disabled" compileBtn
  log "Checking module…"
  msgList <- select "#messages"
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
        select "#compile-status" & setText "OK" & setAttr "class" "label label-success"
        return ()
      CheckError msgs orig -> do
        if all (\(Msg typ _ _) -> typ == MsgWarning) msgs
           then select "#compile-status" & setText "OK" & setAttr "class" "label label-warning"
           else select "#compile-status" & setText "Invalid" & setAttr "class" "label label-important"
        log $ "Check failed: " ++ orig
        when (all (\(Msg typ _ _) -> typ == MsgWarning) msgs) $
          removeAttr "disabled" compileBtn
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

all p (x:xs) = if p x then all p xs else False
all _ [] = True

true :: Fay a -> Fay Bool
true m = m >> return True

unless :: Bool -> Fay () -> Fay ()
unless False m = m
unless _ _ = return ()

hideModal :: JQuery -> Fay ()
hideModal = ffi "%1['modal']('hide')"
