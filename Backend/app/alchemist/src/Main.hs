{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List (dropWhileEnd, isInfixOf, isSuffixOf, sort)
import Data.List.Extra (dropSuffix)
import Data.Text (unpack)
import Kernel.Prelude
import qualified NammaDSL.App as NammaDSL
import System.Directory
import System.Environment (getArgs, setEnv)
import System.FilePath
import Turtle (ExitCode (..), shellStrict)

findGitRoot :: IO (Maybe FilePath)
findGitRoot = do
  (exitStatus, rootpath') <- shellStrict "git rev-parse --show-toplevel" mempty
  return $ case exitStatus of
    ExitSuccess -> Just (dropWhileEnd (== '\n') $ unpack rootpath')
    ExitFailure _ -> Nothing

processSpecFolders :: Bool -> Bool -> FilePath -> IO ()
processSpecFolders isGenAll insideOfSpecFolder specFolderPath =
  if ".direnv" `isInfixOf` specFolderPath
    then putStrLn $ ("ignore folder: " :: String) <> show specFolderPath <> " containing: \".direnv\""
    else processSpecFolders' isGenAll insideOfSpecFolder specFolderPath

processSpecFolders' :: Bool -> Bool -> FilePath -> IO ()
processSpecFolders' isGenAll insideOfSpecDir specFolderPath = do
  contents <- listDirectory specFolderPath
  forM_ contents $ \entry -> do
    let entryPath = specFolderPath </> entry
    isDirectory <- doesDirectoryExist entryPath
    when isDirectory $ do
      let isSpecDir = "/spec" `isSuffixOf` entryPath || insideOfSpecDir
      if isSpecDir
        then do
          let apiFolderPath = entryPath </> "API"
              storageFolderPath = entryPath </> "Storage"
              configPath = entryPath </> "dsl-config.dhall"
          apiContents <- doesDirectoryExist apiFolderPath >>= bool (pure []) (listDirectory apiFolderPath)
          storageContents <- doesDirectoryExist storageFolderPath >>= bool (pure []) (listDirectory storageFolderPath)
          isConfigExists <- doesFileExist configPath
          if not isConfigExists
            then do
              putStrLn' "33" ("Skipping as Config file not found at " ++ configPath)
              processSpecFolders isGenAll isSpecDir entryPath
            else do
              shouldRunApiGenerators <- forM apiContents $
                \inputFile -> do
                  if ".yaml" `isSuffixOf` inputFile
                    then do
                      let inputFilePath = apiFolderPath </> inputFile
                      fileState <- NammaDSL.getFileState inputFilePath
                      -- putStrLn $ show fileState ++ " " ++ inputFilePath
                      let shouldRunApiGenerator = isGenAll || fileState == NammaDSL.NEW || fileState == NammaDSL.CHANGED
                      when shouldRunApiGenerator $
                        NammaDSL.runApiGenerator configPath inputFilePath
                      return shouldRunApiGenerator
                    else return False

              when (or shouldRunApiGenerators && insideOfSpecDir) $ do
                let specModules = map (dropSuffix ".yaml") $ filter (".yaml" `isSuffixOf`) apiContents
                putStrLn $ "run api tree generator for spec modules: " <> (show specModules :: String)
                NammaDSL.runApiTreeGenerator configPath $ sort specModules

              forM_ storageContents $
                \inputFile -> do
                  when (".yaml" `isSuffixOf` inputFile) $ do
                    let inputFilePath = storageFolderPath </> inputFile
                    fileState <- NammaDSL.getFileState inputFilePath
                    -- putStrLn $ show fileState ++ " " ++ inputFilePath
                    when (isGenAll || fileState == NammaDSL.NEW || fileState == NammaDSL.CHANGED) $
                      NammaDSL.runStorageGenerator configPath inputFilePath
        else processSpecFolders isGenAll isSpecDir entryPath

putStrLn' :: String -> String -> IO ()
putStrLn' colorCode text = putStrLn $ "\x1b[" ++ colorCode ++ "m" ++ text ++ "\x1b[0m"

main :: IO ()
main = do
  putStrLn ("Version " ++ NammaDSL.version)
  generateAllSpecs <- ("--all" `elem`) <$> getArgs
  maybeGitRoot <- findGitRoot
  let rootDir = fromMaybe (error "Could not find git root") maybeGitRoot
  setEnv "GIT_ROOT_PATH" (show rootDir)
  putStrLn' "32" ("Root dir: " ++ rootDir)
  processSpecFolders generateAllSpecs False rootDir
