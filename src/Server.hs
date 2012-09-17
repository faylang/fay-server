{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The server.

module Server where

import Server.API
import SharedTypes

import Control.Concurrent
import Control.Monad.IO
import Control.Monad
import Data.Char
import Data.Default
import Data.List
import Data.List.Split
import Data.Maybe
import Data.UUID (UUID)
import Data.UUID.V1
import Language.Fay.Compiler
import Language.Fay.Types
import Safe
import Snap.Core
import Snap.Http.Server
import System.Directory
import System.FilePath
import System.Process.Extra

-- | Main entry point.
server :: IO ()
server = do
  httpServe (setPort 10001 defaultConfig) (route [("/json",handle dispatcher)])

-- | Dispatch on the commands.
dispatcher :: Command -> Dispatcher
dispatcher cmd =
  case cmd of
    GetModule filepath r -> r <~ do
      fmap Text $ io $ readFile filepath

    CheckModule contents r -> r <~ do
      (guid,fpath) <- io $ getTempFile ".hs"
      io $ deleteSoon fpath
      io $ writeFile fpath contents
      result <- io $ typecheck ["include"] [] fpath
      io $ removeFile fpath
      return $
        case result of
          Right _ -> CheckOk (show guid)
          Left orig@(parseMsgs -> msgs) -> CheckError msgs orig

    CompileModule contents r -> r <~ do
      (guid,fpath) <- io $ getTempFile ".hs"
      io $ writeFile fpath contents
      let fout = "static/gen" </> guid ++ ".js"
      result <- io $ compileFromToReturningStatus config fpath fout
      io $ deleteSoon fpath
      io $ deleteSoon fout
      case result of
        Right _ -> do
          io $ generateFile guid
          return (CompileOk guid)
        Left out -> return (CompileError (show out))

  where config = def { configTypecheck = False
                     , configDirectoryIncludes = ["include"]
                     , configPrettyPrint = False
                     }

-- | Generate a HTML file for previewing the generated code.
generateFile :: String -> IO ()
generateFile guid = do
  writeFile fpath $ unlines [
      "<!doctype html>"
    , "<html>"
    , "  <head>"
    ,"    <meta http-equiv='Content-Type' content='text/html; charset=utf-8'>"
    ,"    <link href='/css/bootstrap.min.css' rel='stylesheet'>"
    ,"    <link href='/css/gen.css' rel='stylesheet'>"
    , intercalate "\n" . map ("    "++) $ map makeScriptTagSrc libs
    , intercalate "\n" . map ("    "++) $ map makeScriptTagSrc files
    , "  </head>"
    , "  <body><noscript>Please enable JavaScript.</noscript></body>"
    , "</html>"]
  deleteSoon fpath

  where fpath = "static/gen/" ++ guid ++ ".html"
        makeScriptTagSrc :: FilePath -> String
        makeScriptTagSrc s =
          "<script type=\"text/javascript\" src=\"" ++ s ++ "\"></script>"
        files = [guid ++ ".js"]
        libs = ["/js/jquery.js","/js/date.js"]

-- | Type-check a file.
typecheck :: [FilePath] -> [String] -> String -> IO (Either String (String,String))
typecheck includeDirs ghcFlags fp = do
  readAllFromProcess' "ghc" (
    ["-package","fay","-package-conf","cabal-dev/packages-7.4.2.conf"] ++
    ["-fno-code","-Wall","-Werror",fp] ++
    ["-iinclude"] ++
    map ("-i" ++) includeDirs ++ ghcFlags) ""

-- | Parse the GHC messages.
parseMsgs :: String -> [Msg]
parseMsgs = catMaybes . map parseMsg . splitOn "\n\n"

-- | Parse a GHC message.
parseMsg :: String -> Maybe Msg
parseMsg err = case Msg typ (line err) (dropWarning (message err)) of
  Msg _ 0 "" -> Nothing
  good -> Just good

  where
    line = fromMaybe 0 . readMay . takeWhile isDigit . drop 1 . dropWhile (/=':')
    message = intercalate "\n" . filter (not . null) . map dropIndent . lines
            . drop 1 . dropWhile (/=':') . drop 1 . dropWhile (/=':') . drop 1 . dropWhile (/=':')
    dropWarning x | isPrefixOf wprefix x = drop (length wprefix) x
                  | otherwise = x
    dropIndent x | isPrefixOf "    " x = drop 4 x
                 | otherwise = x
    typ = if isPrefixOf wprefix (message err)
             then MsgWarning
             else MsgError
    wprefix = "Warning: "

-- | Get a unique ID.
getUID :: IO UUID
getUID = do
  uid <- nextUUID
  case uid of
    Nothing -> do threadDelay (1000 * 10)
                  getUID
    Just u -> return u

-- | Get a unique temporary file path with the given extension.
getTempFile :: String -> IO (String,FilePath)
getTempFile ext = do
  dir <- getTemporaryDirectory
  uid <- getUID
  return (show uid,dir </> show uid ++ ext)

-- | Delete the given file soon.
deleteSoon :: FilePath -> IO ()
deleteSoon path = do purge; return () where
  purge = forkIO $ do
    threadDelay (1000 * 1000 * 5)
    exists <- doesFileExist path
    when exists (removeFile path)
