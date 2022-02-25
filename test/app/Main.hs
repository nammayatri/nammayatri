{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Main where

import qualified "app-backend" App as AppBackend
import qualified "beckn-gateway" App as Gateway
import qualified "beckn-transport" App as TransporterBackend
import qualified "fmd-wrapper" App as FmdWrapper
import qualified "mock-fcm" App as MockFcm
import qualified "mock-registry" App as MockRegistry
import qualified "mock-sms" App as MockSms
import qualified "beckn-transport" App.Allocator as Allocator
import qualified "beckn-transport" App.DriverTrackingHealthcheck as DriverHC
import qualified "app-backend" App.Types as AppBackend
import qualified "beckn-transport" App.Types as TransporterBackend
import qualified "fmd-wrapper" App.Types as FmdWrapper
import Beckn.Exit (exitDBMigrationFailure)
import qualified Beckn.Storage.Esqueleto.Migration as Esq
import Beckn.Types.Logging (LoggerConfig)
import Beckn.Utils.App (handleLeft)
import Beckn.Utils.Dhall (readDhallConfigDefault)
import Beckn.Utils.Migration (migrateIfNeeded)
import qualified Data.Text as T (replace, toUpper, unpack)
import EulerHS.Prelude
import qualified FmdWrapper.Spec as FmdWrapper
import GHC.Records.Extra (HasField)
import qualified Mobility.Spec as Mobility
import Resources
import System.Environment as Env (setEnv)
import System.Posix
import Test.Tasty
import TestSilentIOLogger ()
import "app-backend" Types.Geofencing

main :: IO ()
main = do
  -- We can't really spawn off multiple instances of our servers, so serialise...
  Env.setEnv "TASTY_NUM_THREADS" "1"
  -- Set some config paths in environment...
  mapM_
    setConfigEnv
    [ "allocation-service",
      "app-backend",
      "beckn-gateway",
      "beckn-transport",
      "driver-tracking-healthcheck-service",
      "fmd-wrapper",
      "mock-registry"
    ]
  -- ... and run
  defaultMain =<< specs
  where
    setConfigEnv app = do
      Env.setEnv
        (T.unpack $ toEnvVar app <> "_CONFIG_PATH")
        (T.unpack $ "../dhall-configs/dev/" <> app <> ".dhall")
      Env.setEnv
        (T.unpack $ toEnvVar app <> "_MIGRATION_PATH")
        (T.unpack $ "../dev/migrations/" <> app)

    toEnvVar = T.toUpper . T.replace "-" "_"

specs :: IO TestTree
specs = do
  mobilityTests <- Mobility.mkTestTree
  fmdTests <- FmdWrapper.mkTestTree

  return $
    withResource
      (startServers allServers)
      cleanupServers
      ( \_ ->
          testGroup
            "all"
            [ mobilityTests,
              fmdTests
            ]
      )
  where
    allServers =
      [ Allocator.runAllocator \cfg ->
          cfg & hideLogging
            & #driverNotificationExpiry .~ 18
            & #rideAllocationExpiry .~ 18,
        DriverHC.runDriverHealthcheck hideLogging,
        Gateway.runGateway hideLogging,
        AppBackend.runAppBackend $
          \cfg ->
            cfg & hideLogging
              & #geofencingConfig . #origin .~ Regions ["Ernakulam", "Kochi"]
              & #geofencingConfig . #destination .~ Regions ["Kerala", "Kochi"],
        TransporterBackend.runTransporterBackendApp hideLogging,
        FmdWrapper.runFMDWrapper hideLogging,
        MockSms.runMockSms hideLogging,
        MockFcm.runMockFcm hideLogging,
        MockRegistry.runRegistryService hideLogging
      ]

    startServers servers = do
      migrateDB
      prepareTestResources
      traverse_ forkIO servers
      -- Wait for servers to start up and migrations to run
      threadDelay 5e6

    cleanupServers _ = do
      releaseTestResources
      signalProcess sigINT =<< getProcessID

    migrateDB = do
      (appBackendCfg :: AppBackend.AppCfg) <- readDhallConfigDefault "app-backend"
      Esq.migrateIfNeeded (appBackendCfg.migrationPath) (appBackendCfg.esqDBCfg) True
        >>= handleLeft exitDBMigrationFailure "Couldn't migrate app-backend database: "

      (transportCfg :: TransporterBackend.AppCfg) <- readDhallConfigDefault "beckn-transport"
      migrateIfNeeded (transportCfg.migrationPath) (transportCfg.dbCfg) True
        >>= handleLeft exitDBMigrationFailure "Couldn't migrate beckn-transporter database: "

      (fmdCfg :: FmdWrapper.AppCfg) <- readDhallConfigDefault "fmd-wrapper"
      migrateIfNeeded (fmdCfg.migrationPath) (fmdCfg.dbCfg) True
        >>= handleLeft exitDBMigrationFailure "Couldn't migrate fmd-wrapper database: "

hideLogging :: HasField "loggerConfig" cfg LoggerConfig => cfg -> cfg
hideLogging cfg =
  cfg{loggerConfig =
        cfg.loggerConfig
          & #logToConsole .~ False
          & #logRawSql .~ False
     }
