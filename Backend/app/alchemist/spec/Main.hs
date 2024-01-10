{-# OPTIONS_GHC -Wno-unused-matches #-}

module Main where

import Data.IORef
import Kernel.Prelude
import qualified NammaDSL.App as NammaDSL
import System.Directory
import System.Environment as SE
import System.FilePath
import qualified Utils.Types as UT
import Utils.Utils as U

main :: IO ()
main = do
  dhallConfigPath <- SE.getEnv "DHALL_PATH"
  currentDir <- getCurrentDirectory
  maybeGitRoot <- U.findGitRoot currentDir
  let rootDir = fromMaybe (error "Could not find git root") maybeGitRoot
  dhallConfigsRef <- U.readAndStoreAlchemistConfig dhallConfigPath
  configs <- readIORef dhallConfigsRef

  processApp rootDir (UT.riderApp configs)
  where
    processApp :: FilePath -> UT.AppConfigs -> IO ()
    processApp rootDir appPath = do
      let
      U.applyDirectory (rootDir </> (UT.storageYaml $ UT.inputFileConfigs appPath)) (processStorageDSL rootDir appPath)
      U.applyDirectory (rootDir </> (UT.apiYaml $ UT.inputFileConfigs appPath)) (processAPIDSL rootDir appPath)

    processStorageDSL rootDir appPath inputFile = do
      NammaDSL.mkBeamTable (UT.beamTableOutputFilePath $ UT.outputFileConfigs appPath) inputFile
      NammaDSL.mkBeamQueries (UT.beamQueriesOutputFilePath $ UT.outputFileConfigs appPath) inputFile
      NammaDSL.mkDomainType (UT.domainTypeOutputFilePath $ UT.outputFileConfigs appPath) inputFile
      NammaDSL.mkSQLFile (UT.sqlOutputFilePath $ UT.outputFileConfigs appPath) inputFile

    processAPIDSL rootDir appPath inputFile = do
      -- NammaDSL.mkFrontendAPIIntegration (readOnlySrc </> "Domain/Action") inputFile
      NammaDSL.mkServantAPI (UT.servantAPIOutputFilePath $ UT.outputFileConfigs appPath) inputFile
      NammaDSL.mkApiTypes (UT.apiTypesOutputFilePath $ UT.outputFileConfigs appPath) inputFile
      NammaDSL.mkDomainHandler (UT.domainHandlerOutputFilePath $ UT.outputFileConfigs appPath) inputFile
