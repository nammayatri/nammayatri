module Spec (generateCode) where

import qualified API.TicketBooking as AT
import Kernel.Prelude
import qualified Storage.TicketBooking as ST
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
  let targetFolder = rootDir </> "Backend/app/rider-platform/rider-app/Main/src/"
  ST.mkStorage targetFolder
  AT.mkAPI targetFolder
