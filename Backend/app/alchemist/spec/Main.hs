module Main where

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

dslInputPathPrefix :: FilePath
dslInputPathPrefix = "Backend" </> "app" </> "alchemist" </> "spec"

haskellOutputPathPrefix :: FilePath
haskellOutputPathPrefix = "Backend" </> "app"

sqlOutputPathPrefix :: FilePath
sqlOutputPathPrefix = "Backend" </> "dev" </> "migrations-read-only"

rideAppName :: FilePath
rideAppName = "rider-app"

driverAppName :: FilePath
driverAppName = "dynamic-offer-driver-app"

riderAppPath :: FilePath
riderAppPath = "rider-platform" </> rideAppName </> "Main"

driverAppPath :: FilePath
driverAppPath = "provider-platform" </> driverAppName </> "Main"

applyDirectory :: FilePath -> (FilePath -> IO ()) -> IO ()
applyDirectory dirPath processFile = do
  files <- listDirectory dirPath
  let yamlFiles = filter (\file -> takeExtension file `elem` [".yaml", ".yml"]) files
  mapM_ (processFile . (dirPath </>)) yamlFiles

main :: IO ()
main = do
  currentDir <- getCurrentDirectory
  maybeGitRoot <- findGitRoot currentDir
  let rootDir = fromMaybe (error "Could not find git root") maybeGitRoot

  processApp rootDir riderAppPath rideAppName
  processApp rootDir driverAppPath driverAppName
  where
    processApp :: FilePath -> FilePath -> FilePath -> IO ()
    processApp rootDir appPath appName = do
      applyDirectory (rootDir </> dslInputPathPrefix </> appPath </> "Storage") (processStorageDSL rootDir appPath appName)
      applyDirectory (rootDir </> dslInputPathPrefix </> appPath </> "API") (processAPIDSL rootDir appPath)

    processStorageDSL rootDir appPath appName inputFile = do
      let readOnlySrc = rootDir </> haskellOutputPathPrefix </> appPath </> "src-read-only/"
      let readOnlyMigration = rootDir </> sqlOutputPathPrefix </> appName

      Alchemist.mkBeamTable (readOnlySrc </> "Storage/Beam") inputFile
      Alchemist.mkBeamQueries (readOnlySrc </> "Storage/Queries") inputFile
      Alchemist.mkDomainType (readOnlySrc </> "Domain/Types") inputFile
      Alchemist.mkSQLFile readOnlyMigration inputFile

    processAPIDSL rootDir appPath inputFile = do
      let readOnlySrc = rootDir </> haskellOutputPathPrefix </> appPath </> "src-read-only/"
      let src = rootDir </> haskellOutputPathPrefix </> appPath </> "src"

      Alchemist.mkServantAPI (readOnlySrc </> "API/Action/UI") inputFile
      Alchemist.mkApiTypes (readOnlySrc </> "API/Types/UI") inputFile
      Alchemist.mkDomainHandler (src </> "Domain/Action/UI") inputFile

-- Alchemist.mkFrontendAPIBackend (targetFolder </> "Domain/Action") inputFile
-- Alchemist.mkFrontendAPIEndpoint (targetFolder </> "Domain/Action") inputFile
