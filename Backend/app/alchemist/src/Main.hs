module Main where

import qualified Data.ByteString as BS
import qualified Data.Yaml as Yaml
import Kernel.Prelude
import qualified NammaDSL.App as NammaDSL
import System.Directory
import System.Environment (getArgs)
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

data Apps = DriverApp | RiderApp deriving (Generic, FromJSON, Show)

data LibraryPaths = LibraryPaths
  { libPath :: FilePath,
    usedIn :: [Apps]
  }
  deriving (Generic, FromJSON, Show)

getFilePathsForConfiguredApps :: FilePath -> IO [LibraryPaths]
getFilePathsForConfiguredApps rootDir = do
  contents <- BS.readFile $ rootDir </> "Backend/dslLibs.yaml"
  case Yaml.decodeEither' contents of
    Left err -> error $ show err
    Right yml -> return yml

main :: IO ()
main = do
  putStrLn ("Version " ++ NammaDSL.version)
  generateAllSpecs <- ("--all" `elem`) <$> getArgs
  currentDir <- getCurrentDirectory
  maybeGitRoot <- findGitRoot currentDir
  let rootDir = fromMaybe (error "Could not find git root") maybeGitRoot

  processApp generateAllSpecs riderAppDatabaseName rootDir riderAppPath rideAppName
  processApp generateAllSpecs driverAppDatabaseName rootDir driverAppPath driverAppName
  paths <- getFilePathsForConfiguredApps rootDir
  mapM_
    ( \libPaths ->
        mapM_
          ( \lib -> do
              let (databaseName, appName) =
                    case lib of
                      DriverApp -> (driverAppDatabaseName, driverAppName)
                      RiderApp -> (riderAppDatabaseName, riderAppDatabaseName)
              processApp generateAllSpecs databaseName rootDir libPaths.libPath appName
          )
          libPaths.usedIn
    )
    paths
  where
    processApp :: Bool -> String -> FilePath -> FilePath -> FilePath -> IO ()
    processApp isGenAll dbName rootDir appPath appName = do
      applyDirectory (rootDir </> appPath </> "spec" </> "Storage") (processStorageDSL isGenAll dbName rootDir appPath appName)
      applyDirectory (rootDir </> appPath </> "spec" </> "API") (processAPIDSL isGenAll rootDir appPath)

    processStorageDSL isGenAll dbName' rootDir appPath appName inputFile = do
      fileState <- NammaDSL.getFileState inputFile
      putStrLn $ show fileState ++ " " ++ inputFile
      when (isGenAll || fileState == NammaDSL.NEW || fileState == NammaDSL.CHANGED) $ do
        let readOnlySrc = rootDir </> appPath </> "src-read-only/"
        let src = rootDir </> appPath </> "src"
        let readOnlyMigration = rootDir </> sqlOutputPathPrefix </> appName

        NammaDSL.mkBeamTable (readOnlySrc </> "Storage/Beam") inputFile
        NammaDSL.mkBeamQueries (readOnlySrc </> "Storage/Queries") (Just (src </> "Storage/Queries")) inputFile
        NammaDSL.mkDomainType (readOnlySrc </> "Domain/Types") inputFile
        NammaDSL.mkSQLFile (Just dbName') readOnlyMigration inputFile

    processAPIDSL isGenAll rootDir appPath inputFile = do
      fileState <- NammaDSL.getFileState inputFile
      putStrLn $ show fileState ++ " " ++ inputFile
      when (isGenAll || fileState == NammaDSL.NEW || fileState == NammaDSL.CHANGED) $ do
        let readOnlySrc = rootDir </> appPath </> "src-read-only/"
        let src = rootDir </> appPath </> "src"

        -- NammaDSL.mkFrontendAPIIntegration (readOnlySrc </> "Domain/Action") inputFile
        NammaDSL.mkServantAPI (readOnlySrc </> "API/Action/UI") inputFile
        NammaDSL.mkApiTypes (readOnlySrc </> "API/Types/UI") inputFile
        NammaDSL.mkDomainHandler (src </> "Domain/Action/UI") inputFile
