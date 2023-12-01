module Spec (generateCode) where

import qualified Alchemist.App as Alchemist
import Kernel.Prelude
import System.Directory
import System.FilePath

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

generateCode :: IO ()
generateCode = do
  currentDir <- getCurrentDirectory
  maybeGitRoot <- findGitRoot currentDir
  let rootDir = fromMaybe (error "Could not find git root") maybeGitRoot
  let targetFolder = rootDir </> "Backend/app/rider-platform/rider-app/Main/src-read-only/"
  let inputFolder = rootDir </> "Backend/app/rider-platform/rider-app/Main/spec/"
  let inputFile = inputFolder </> "Storage/ticket.yaml"
  let apiInputFile = inputFolder </> "API/ticket.yaml"
  Alchemist.mkBeamTable (targetFolder </> "Storage/Beam") inputFile
  Alchemist.mkBeamQueries (targetFolder </> "Storage/Queries") inputFile
  Alchemist.mkDomainType (targetFolder </> "Domain/Types") inputFile
  Alchemist.mkSQLFile targetFolder inputFile
  Alchemist.mkFrontendAPIBackend (targetFolder </> "Domain/Action") apiInputFile
  Alchemist.mkFrontendAPIEndpoint (targetFolder </> "Domain/Action") apiInputFile
