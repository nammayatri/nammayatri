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

becknSpecLibOutputPathPrefix :: FilePath
becknSpecLibOutputPathPrefix = "Backend" </> "lib" </> "beckn-spec"

becknOnDemandTransformerPath :: FilePath
becknOnDemandTransformerPath = "Beckn" </> "OnDemand" </> "Transformer"

sqlOutputPathPrefix :: FilePath
sqlOutputPathPrefix = "Backend" </> "dev" </> "migrations-read-only"

riderAppName :: FilePath
riderAppName = "rider-app"

driverAppName :: FilePath
driverAppName = "dynamic-offer-driver-app"

riderAppPath :: FilePath
riderAppPath = "rider-platform" </> riderAppName </> "Main"

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

  processApp rootDir riderAppPath riderAppName
  where
    -- processApp rootDir driverAppPath driverAppName

    processApp :: FilePath -> FilePath -> FilePath -> IO ()
    processApp rootDir appPath _appName = do
      -- applyDirectory (rootDir </> dslInputPathPrefix </> appPath </> "Storage") (processStorageDSL rootDir appPath appName)
      -- applyDirectory (rootDir </> dslInputPathPrefix </> appPath </> "API") (processAPIDSL rootDir appPath)
      applyDirectory (rootDir </> dslInputPathPrefix </> appPath </> becknOnDemandTransformerPath) (processBecknACL rootDir appPath)

    -- processStorageDSL rootDir appPath appName inputFile = do
    --   let readOnlySrc = rootDir </> haskellOutputPathPrefix </> appPath </> "src-read-only/"
    --   let readOnlyMigration = rootDir </> sqlOutputPathPrefix </> appName

    --   Alchemist.mkBeamTable (readOnlySrc </> "Storage/Beam") inputFile
    --   Alchemist.mkBeamQueries (readOnlySrc </> "Storage/Queries") inputFile
    --   Alchemist.mkDomainType (readOnlySrc </> "Domain/Types") inputFile
    --   Alchemist.mkSQLFile readOnlyMigration inputFile

    -- processAPIDSL rootDir appPath inputFile = do
    --   let readOnlySrc = rootDir </> haskellOutputPathPrefix </> appPath </> "src-read-only/"
    --   let src = rootDir </> haskellOutputPathPrefix </> appPath </> "src"

    --   Alchemist.mkServantAPI (readOnlySrc </> "API/Action/UI") inputFile
    --   Alchemist.mkApiTypes (readOnlySrc </> "API/Types/UI") inputFile
    --   Alchemist.mkDomainHandler (src </> "Domain/Action/UI") inputFile

    processBecknACL rootDir appPath inputFile = do
      let readOnlySrc = rootDir </> haskellOutputPathPrefix </> appPath </> "src-read-only/"

      Alchemist.mkTransformerFunctions (readOnlySrc </> becknOnDemandTransformerPath) inputFile

-- Alchemist.mkFrontendAPIBackend (targetFolder </> "Domain/Action") inputFile
-- Alchemist.mkFrontendAPIEndpoint (targetFolder </> "Domain/Action") inputFile
