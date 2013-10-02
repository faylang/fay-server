{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}
{-# OPTIONS -fno-warn-name-shadowing #-}

-- | The server.

module Server where

import           Server.API
import           SharedTypes

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO
import           Data.Char
import           Data.Default
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Data.UUID             (UUID)
import           Data.UUID.V1
import           Fay
import           Fay.Compiler.Config
import           Language.Haskell.Exts
import           Safe
import           Snap.Core
import           Snap.Http.Server
import           Snap.Util.FileServe
import           System.Directory
import           System.FilePath
import qualified System.IO.Strict      as S
import           System.Process.Extra
import qualified GHC.Paths as GHCPaths

-- | Main entry point.
server :: IO ()
server = do
  httpServe (setPort 10001 defaultConfig) (route [("/json",handle dispatcher),("",serveDirectory "static")])

-- | Dispatch on the commands.
dispatcher :: Command -> Dispatcher
dispatcher cmd =
  case cmd of

    LibraryModules r -> r <~ do
      io $ getModulesIn "library"
    ProjectModules r -> r <~ do
      io $ getModulesIn "project"
    GlobalModules r -> r <~ do
      io $ getModulesIn "global"
    InternalModules r -> r <~ do
      return (ModuleList allowedInternals)

    GetModule mname r -> r <~ do
      t <- io $ loadModule mname
      return $ case t of
        Nothing -> NoModule mname
        Just (_,x) -> LoadedModule x

    CheckModule (stripTabs -> contents) r -> r <~ do
      modules <- io $ getAllModules
      sanitize modules contents $ do
        (guid,fpath) <- io $ getTempFile ".hs"
        io $ deleteSoon fpath
        let (skiplines,out) = formatForGhc contents
        io $ writeFile fpath out
        dirs <- io getModuleDirs
        result <- io $ typecheck config fpath
        io $ removeFile fpath
        return $
          case result of
            Right _ -> CheckOk (show guid)
            Left orig@(parseMsgs skiplines -> msgs) -> CheckError msgs orig

    CompileModule (stripTabs -> contents) r -> r <~ do
      let moduleName = getModuleName contents
      maybeModule <- io $ loadModule moduleName
      (guid,tmpPath) <- io $ getTempFile ".hs"
      let returnTmp = do io $ deleteSoon tmpPath
                         return tmpPath
      fpath <- case maybeModule of
        Just (path,_) -> if isInfixOf "global" (takeDirectory path)
                            then return path
                            else returnTmp
        Nothing -> returnTmp
      io $ writeFile fpath contents
      let fout = "static/gen" </> guid ++ ".js"
      result <- io $ compileFromToAndGenerateHtml config fpath fout
      io $ deleteSoon fout
      case result of
        Right _ -> do
          io $ generateFile (getModuleName contents) guid
          return (CompileOk guid)
        Left out -> return (CompileError (showCompileError out))

    CleanModuleName name r -> r <~ do
      let normalizedName = normalizeModule name
      validateModule normalizedName

    CreateModule name r -> r <~ do
      r <- validateModule name
      case r of
        CleanModule name -> do
          let fp = "modules/global/" ++ moduleToFile name
          io $ createDirectoryIfMissing True (takeDirectory fp)
          io $ writeFile fp (moduleTemplate name)
        _ -> return ()
      return r

config :: CompileConfig
config = addConfigDirectoryIncludePaths ["modules/library"
                                        ,"modules/project"
                                        ,"src"] def { configTypecheck = False, configPrettyPrint = True }

moduleTemplate :: [Char] -> String
moduleTemplate name = unlines
  ["module " ++ name ++ " where"
  ,""
  ,"import Language.Fay.Prelude"
  ,"import Language.Fay.FFI"
  ,"import Language.Fay.DOM"
  ,"import Language.Fay.JQuery"
  ]

validateModule :: String -> Snap ModuleNameCheck
validateModule normalizedName =
   case parseModule ("module " ++ normalizedName ++ " where") of
     ParseOk{} -> do
       if not (any (=='.') normalizedName)
          then return (InvalidModule "Please use `Yourname.Whatever'")
          else do
            t <- io $ loadModule normalizedName
            return $ case t of
              Nothing -> CleanModule normalizedName
              Just{} -> InvalidModule "Already in use: sorry! Try `YourName.Something'"
     ParseFailed _ err -> return (InvalidModule err)

-- | Normalize a module name, to help newbies.
normalizeModule :: String -> String
normalizeModule = intercalate "." . map upperize . splitWhen (=='.')
  where upperize (c:cs) = toUpper c : cs
        upperize [] = []

stripTabs :: [Char] -> [Char]
stripTabs ('\t':cs) = "    " ++ stripTabs cs
stripTabs (c:cs) = c : stripTabs cs
stripTabs [] = []

sanitize :: [ModuleName] -> String -> Snap CheckResult -> Snap CheckResult
sanitize modules x m = do
  case verify modules x of
    Nothing -> m
    Just prob -> return $ CheckError [prob] x

getModuleName :: String -> String
getModuleName source =
  case parseModule source of
    ParseFailed{} -> "Main"
    ParseOk (Module _ (ModuleName name) _ _ _ _ _) ->
      name

getImports :: String -> [ImportDecl]
getImports source =
  case parseModule source of
    ParseFailed{} -> []
    ParseOk (Module _ _ _ _ _ imports _) -> imports

verify :: [ModuleName] -> String -> Maybe Msg
verify modules source =
  case parseModule source of
    ParseFailed SrcLoc{srcLine} err -> Just $
      Msg MsgError
          (fromIntegral srcLine)
          err
    ParseOk (Module srcloc _ pragmas _ exports (filter languageFay -> imports) _) ->
      case find (not . flip elem modules . importModule) imports of
        Just ImportDecl{importModule=ModuleName mname,importLoc=SrcLoc{..}} -> Just $
          Msg MsgError
              (fromIntegral srcLine)
              ("no such Fay module: " ++ mname)
        Nothing ->
          case exports of
            Just (_:_) -> Just $
              Msg MsgError
                  (fromIntegral (srcLine srcloc))
                  "export specs aren't supported"
            _ ->
              case find badPragma pragmas of
                Just pragma -> Just $
                  Msg MsgError
                      (pragmaLine pragma)
                      ("unsupported pragma " ++ prettyPrint pragma)
                Nothing -> Nothing

  where languageFay (importModule -> ModuleName x) =
          not (isPrefixOf "Language.Fay." x)
        badPragma p =
          case p of
            LanguagePragma _ names -> any (not . flip elem oklanguages) names
            _ -> True
        pragmaLine p = fromIntegral $ srcLine $
          case p of
            LanguagePragma s _ -> s
            OptionsPragma s _ _ -> s
            AnnModulePragma s _ -> s

oklanguages :: [Name]
oklanguages =
  [Ident "NoImplicitPrelude"
  ,Ident "GADTs"
  ,Ident "EmptyDataDecls"
  ,Ident "IncoherentInstances"
  ,Ident "FlexibleInstances"
  ,Ident "RankNTypes"]

getAllModules :: IO [ModuleName]
getAllModules = do
  ModuleList l <- getModulesIn "library"
  ModuleList p <- getModulesIn "project"
  return (map ModuleName (l ++ p ++ allowedInternals))

allowedInternals :: [String]
allowedInternals = ["SharedTypes","Client.API"]
allowedInternalDirs :: [FilePath]
allowedInternalDirs = ["src"]

getModulesIn :: FilePath -> IO ModuleList
getModulesIn dr = do
  fmap (ModuleList . sort . map fileToModule)
       (getDirectoryItemsRecursive ("modules" </> dr))

getModuleDirs :: IO [FilePath]
getModuleDirs = do
  ms <- getDirectoryItems "modules"
  return (ms ++ allowedInternalDirs)

getDirectoryItems :: FilePath -> IO [FilePath]
getDirectoryItems dr =
  fmap (map (dr </>) . filter (not . all (=='.')))
       (getDirectoryContents dr)

getDirectoryItemsRecursive :: FilePath -> IO [FilePath]
getDirectoryItemsRecursive dr = do
  elems <- fmap (filter (not . all (=='.'))) (getDirectoryContents dr)
  dirs <- filterM doesDirectoryExist . map (dr </>) $ elems
  files <- return (filter (not . flip elem dirs . (dr </>)) elems)
  subdirs <- mapM getDirectoryItemsRecursive dirs
  return (map (dr </>) files ++ concat subdirs)

-- | Load a module from a module name.
loadModule :: String -> IO (Maybe (String,String))
loadModule mname = do
  dirs <- getModuleDirs
  tries <- mapM try (map (</> moduleToFile mname) dirs)
  return (listToMaybe (catMaybes tries))

  where
        try fpath = do
          exists <- doesFileExist fpath
          if exists
             then fmap (\x -> Just (fpath,x)) (S.readFile fpath)
             else return Nothing

fileToModule :: [Char] -> [Char]
fileToModule = go . dropWhile (not . isUpper) where
  go ('/':cs) = '.' : go cs
  go ".hs"    = ""
  go (c:cs)   = c   : go cs
  go x        = x

-- | Convert a module name to a file name.
moduleToFile :: [Char] -> [Char]
moduleToFile = go where
  go ('.':cs) = '/' : go cs
  go (c:cs)   = c   : go cs
  go []       = ".hs"

-- | Format a Fay module for GHC.
formatForGhc :: String -> (Int,String)
formatForGhc contents =
  (length ls,unlines ls ++ contents)
  where ls = ["{-# OPTIONS -fno-warn-type-defaults -fno-warn-orphans -fno-warn-missing-signatures #-}"
             ,"{-# LANGUAGE NoImplicitPrelude #-}"]

-- | Generate a HTML file for previewing the generated code.
generateFile :: String -> String -> IO ()
generateFile mname guid = do
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
        libs = ["/js/jquery.js","/js/date.js","/js/gen.js","/js/jquery.cookie.js"] ++
               ["/js/three.min.js" | mname == "Demo.Three"]


-- | Type-check a file.
typecheck :: CompileConfig -> FilePath -> IO (Either String (String,String))
typecheck cfg fp = do
  let faydir = "foo"
  let includes = configDirectoryIncludes cfg

  -- Remove the fay source dir from includeDirs to prevent errors on FFI instance declarations.
  let includeDirs = map snd . filter ((/= faydir) . snd) . filter (isNothing . fst) $ includes
  let packages = nub . map (fromJust . fst) . filter (isJust . fst) $ includes

  let flags =
          [ "-fno-code"
          , "-XNoImplicitPrelude"
          , "-hide-package base"
          , "-cpp", "-pgmPcpphs", "-optP--cpp", "-DFAY=1"
          , "-main-is"
          , "Language.Fay.DummyMain"
          , "-i" ++ intercalate ":" includeDirs
          , fp ]
  readAllFromProcess' GHCPaths.ghc flags ""

-- | Parse the GHC messages.
parseMsgs :: Int -> String -> [Msg]
parseMsgs skiplines = catMaybes . map (parseMsg skiplines) . splitOn "\n\n"

-- | Parse a GHC message.
parseMsg :: Int -> String -> Maybe Msg
parseMsg skiplines err = case Msg typ (line err) (dropWarning (message err)) of
  Msg _ n "" | n < 1 -> Nothing
  good -> Just good

  where
    line = subtract (fromIntegral skiplines) . fromMaybe 0 . readMay
         . takeWhile isDigit . drop 1 . dropWhile (/=':')
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
  dr <- getTemporaryDirectory
  uid <- getUID
  return (show uid,dr </> show uid ++ ext)

-- | Delete the given file soon.
deleteSoon :: FilePath -> IO ()
deleteSoon p = do _ <- purge; return () where
  purge = forkIO $ do
    threadDelay (1000 * 1000 * 5)
    exists <- doesFileExist p
    when exists (removeFile p)
