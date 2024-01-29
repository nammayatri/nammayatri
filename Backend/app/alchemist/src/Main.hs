module Main where

import Kernel.Prelude
import qualified NammaDSL.App as NammaDSL
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

sqlOutputPathPrefix :: FilePath
sqlOutputPathPrefix = "Backend" </> "dev" </> "migrations-read-only"

rideAppName :: FilePath
rideAppName = "rider-app"

driverAppName :: FilePath
driverAppName = "dynamic-offer-driver-app"

riderAppDatabaseName :: String
riderAppDatabaseName = "atlas_app"

driverAppDatabaseName :: String
driverAppDatabaseName = "atlas_driver_offer_bpp"

riderAppPath :: FilePath
riderAppPath = "Backend" </> "app" </> "rider-platform" </> rideAppName </> "Main"

driverAppPath :: FilePath
driverAppPath = "Backend" </> "app" </> "provider-platform" </> driverAppName </> "Main"

applyDirectory :: FilePath -> (FilePath -> IO ()) -> IO ()
applyDirectory dirPath processFile = do
  exists <- doesDirectoryExist dirPath
  when exists $ do
    files <- listDirectory dirPath
    let yamlFiles = filter (\file -> takeExtension file `elem` [".yaml", ".yml"]) files
    mapM_ (processFile . (dirPath </>)) yamlFiles

main :: IO ()
main = do
  putStrLn ("Version " ++ NammaDSL.version)
  currentDir <- getCurrentDirectory
  maybeGitRoot <- findGitRoot currentDir
  let rootDir = fromMaybe (error "Could not find git root") maybeGitRoot

  processApp riderAppDatabaseName rootDir riderAppPath rideAppName
  processApp driverAppDatabaseName rootDir driverAppPath driverAppName
  where
    processApp :: String -> FilePath -> FilePath -> FilePath -> IO ()
    processApp dbName rootDir appPath appName = do
      applyDirectory (rootDir </> appPath </> "spec" </> "Storage") (processStorageDSL dbName rootDir appPath appName)
      applyDirectory (rootDir </> appPath </> "spec" </> "API") (processAPIDSL rootDir appPath)

    processStorageDSL dbName' rootDir appPath appName inputFile = do
      let readOnlySrc = rootDir </> appPath </> "src-read-only/"
      let src = rootDir </> appPath </> "src"
      let readOnlyMigration = rootDir </> sqlOutputPathPrefix </> appName

      NammaDSL.mkBeamTable (readOnlySrc </> "Storage/Beam") inputFile
      NammaDSL.mkBeamQueries (readOnlySrc </> "Storage/Queries") (Just (src </> "Storage/Queries")) inputFile
      NammaDSL.mkDomainType (readOnlySrc </> "Domain/Types") inputFile
      NammaDSL.mkSQLFile (Just dbName') readOnlyMigration inputFile

    processAPIDSL rootDir appPath inputFile = do
      let readOnlySrc = rootDir </> appPath </> "src-read-only/"
      let src = rootDir </> appPath </> "src"

      -- NammaDSL.mkFrontendAPIIntegration (readOnlySrc </> "Domain/Action") inputFile
      NammaDSL.mkServantAPI (readOnlySrc </> "API/Action/UI") inputFile
      NammaDSL.mkApiTypes (readOnlySrc </> "API/Types/UI") inputFile
      NammaDSL.mkDomainHandler (src </> "Domain/Action/UI") inputFile
