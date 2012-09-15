{-# LANGUAGE ViewPatterns #-}

-- | Build system including the client Fay code.

module Main where

import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.LocalBuildInfo
import Distribution.ModuleName (ModuleName,toFilePath)

import Control.Monad
import Language.Fay.Compiler
import Language.Fay.Types
import System.FilePath
import System.Directory
import Data.Default

-- | Install some hooks.
main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { preBuild  = note
  , postBuild = buildFay
  }

-- | Just a little note so the output looks nice.
note :: Args -> BuildFlags -> IO HookedBuildInfo
note _ _ = do
  putStrLn "Building the server ..."
  return emptyHookedBuildInfo

-- | Build the client.
buildFay :: Args -> BuildFlags -> PackageDescription -> LocalBuildInfo -> IO ()
buildFay _ _ pkgdesc buildinfo = do
  putStrLn "Building the client ..."
  case library pkgdesc of
    Nothing -> error "Need a library in the Cabal file!"
    Just library ->
      forM_ (exposedModules library) $ \(moduleNameToPath -> path) ->
        forM_ (hsSourceDirs (libBuildInfo library)) $ \dir -> do
          let candidate = dir </> path
              PackageName name = pkgName (package pkgdesc)
              -- Figure out a good place to put this in the .cabal.
              out = "static/js" </> name ++ ".js"
          exists <- doesFileExist candidate
          when exists $ do
            putStrLn $ "Compiling " ++ candidate ++ " to " ++ out ++ " ..."
            compileFromTo (config dir) candidate out

     where moduleNameToPath md = toFilePath md ++ ".hs"
           config dir = def
             { configInlineForce       = False
             , configFlattenApps       = True
             , configExportBuiltins    = True
             , configDirectoryIncludes = [dir]
             , configPrettyPrint       = True
             , configAutorun           = True
             , configHtmlWrapper       = False
             }
