module Utils.Utils where

import Data.IORef
import Kernel.Prelude
import Kernel.Utils.Dhall (readDhallConfig)
import System.Directory
import System.FilePath
import qualified Utils.Types as UT

-- Function to read the Dhall configuration and store it in an IORef
readAndStoreAlchemistConfig :: FilePath -> IO (IORef UT.AlchemistConfigs)
readAndStoreAlchemistConfig filePath = do
  configs <- readDhallConfig filePath
  newIORef configs

findGitRoot :: FilePath -> IO (Maybe FilePath)
findGitRoot dir = do
  let gitPath = dir </> ".git"
  exists <- doesDirectoryExist gitPath
  if exists
    then return (Just dir)
    else
      let parent = takeDirectory dir
       in if parent == dir
            then return Nothing -- No more directories to check
            else findGitRoot parent

applyDirectory :: FilePath -> (FilePath -> IO ()) -> IO ()
applyDirectory dirPath processFile = do
  files <- listDirectory dirPath
  let yamlFiles = filter (\file -> takeExtension file `elem` [".yaml", ".yml"]) files
  mapM_ (processFile . (dirPath </>)) yamlFiles
