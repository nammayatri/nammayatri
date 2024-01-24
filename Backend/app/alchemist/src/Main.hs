module Main where

-- import Data.IORef
import Kernel.Prelude
import qualified NammaDSL.App as NammaDSL
-- import System.Directory
import System.Environment as SE
-- import System.FilePath
-- import qualified Utils.Types as UT
import Utils.Utils as U

-- import Kernel.Utils.Dhall (readDhallConfig)

main :: IO ()
main = do
  dhallConfigPath <- SE.getEnv "DHALL_PATH"
  configs <- U.readAndStoreDhallConfig dhallConfigPath
  NammaDSL.runGeneration configs

-- currentDir <- getCurrentDirectory
-- maybeGitRoot <- U.findGitRoot currentDir
-- let rootDir = fromMaybe (error "Could not find git root") maybeGitRoot

-- configs <- readIORef dhallConfigsRef

-- processApp rootDir (UT.riderApp configs)
-- processApp rootDir (UT.driverApp configs)
-- where
--   processApp :: FilePath -> UT.AppConfigs -> IO ()
--   processApp rootDir appPath = do
--     let
--     U.applyDirectory (rootDir </> (UT.storageYaml $ UT.inputFileConfigs appPath)) (processStorageDSL rootDir appPath)
--     U.applyDirectory (rootDir </> (UT.apiYaml $ UT.inputFileConfigs appPath)) (processAPIDSL rootDir appPath)

--   processStorageDSL rootDir appPath inputFile = do
--     NammaDSL.mkBeamTable (rootDir </> (UT.beamTableOutputFilePath $ UT.outputFileConfigs appPath)) inputFile
--     NammaDSL.mkBeamQueries (rootDir </> (UT.beamQueriesOutputFilePath $ UT.outputFileConfigs appPath)) inputFile
--     NammaDSL.mkDomainType (rootDir </> (UT.domainTypeOutputFilePath $ UT.outputFileConfigs appPath)) inputFile
--     NammaDSL.mkSQLFile (rootDir </> (UT.sqlOutputFilePath $ UT.outputFileConfigs appPath)) inputFile

--   processAPIDSL rootDir appPath inputFile = do
--     -- NammaDSL.mkFrontendAPIIntegration (readOnlySrc </> "Domain/Action") inputFile
--     NammaDSL.mkServantAPI (rootDir </> (UT.servantAPIOutputFilePath $ UT.outputFileConfigs appPath)) inputFile
--     NammaDSL.mkApiTypes (rootDir </> (UT.apiTypesOutputFilePath $ UT.outputFileConfigs appPath)) inputFile
--     NammaDSL.mkDomainHandler (rootDir </> (UT.domainHandlerOutputFilePath $ UT.outputFileConfigs appPath)) inputFile
