{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Main where

import qualified "app-backend" App as AppBackend
import qualified "beckn-gateway" App as Gateway
import qualified "beckn-transport" App as TransporterBackend
import qualified "beckn-transport-allocator" App as Allocator
import qualified "beckn-transport-driver-tracking-health-check" App as DriverHC
import qualified "driver-offer-bpp" App as DriverOfferBpp
import qualified "mock-fcm" App as MockFcm
import qualified "mock-public-transport-bpp" App as MockPublicTransportBpp
import qualified "mock-registry" App as MockRegistry
import qualified "mock-sms" App as MockSms
import qualified "public-transport-bap" App as PublicTransport
import qualified "public-transport-search-consumer" App as PublicTransportSearchConsumer
import qualified "search-result-aggregator" App as SearchResultAggregator
import qualified "app-backend" App.Types as AppBackend
import qualified "beckn-transport" App.Types as TransporterBackend
import Beckn.Exit (exitDBMigrationFailure)
import qualified Beckn.Storage.Esqueleto.Migration as Esq
import Beckn.Types.Geofencing
import Beckn.Types.Logging (LoggerConfig)
import Beckn.Utils.App (handleLeft)
import Beckn.Utils.Dhall (readDhallConfigDefault)
import qualified Data.Text as T (replace, toUpper, unpack)
import qualified "driver-offer-bpp" Environment as DriverOfferBpp
import EulerHS.Prelude
import GHC.Records.Extra (HasField)
import qualified Mobility.ARDU.Spec as Mobility.ARDU
import Mobility.Fixtures.AppBackend
import Mobility.Fixtures.Common
import qualified Mobility.Transporter.Spec as Mobility.Transporter
import PublicTransport.Common
import qualified PublicTransport.Spec as PublicTransport
import Resources
import System.Environment as Env (setEnv)
import System.Posix
import Test.Tasty
import TestSilentIOLogger ()

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
      "mock-registry",
      "public-transport-bap",
      "mock-public-transport-bpp",
      "public-transport-search-consumer",
      "search-result-aggregator",
      "driver-offer-bpp"
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
specs =
  specs'
    --    [Transporter.Mobility.mkTestTree, PublicTransport.mkTestTree]
    [Mobility.ARDU.mkTestTree]

specs' :: [IO TestTree] -> IO TestTree
specs' trees = do
  readyTests <- sequence trees
  return $
    withResource
      (startServers allServers)
      cleanupServers
      ( \_ ->
          testGroup
            "all"
            readyTests
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
        TransporterBackend.runTransporterBackendApp $ \cfg ->
          cfg & hideLogging
            & #updateLocationRefreshPeriod .~ timeBetweenLocationUpdates,
        MockSms.runMockSms hideLogging,
        MockFcm.runMockFcm hideLogging,
        MockRegistry.runRegistryService hideLogging,
        PublicTransport.runService $ \cfg ->
          cfg & hideLogging,
        MockPublicTransportBpp.runMock $ \cfg ->
          cfg & #statusWaitTimeSec .~ mockWaitTimeSeconds
            & hideLogging,
        PublicTransportSearchConsumer.runPublicTransportSearchConsumer $ \cfg ->
          cfg & hideLogging
            & #kafkaConsumerCfgs . #publicTransportSearch . #timeoutMilliseconds .~ kafkaConsumerTimeoutMilliseconds,
        SearchResultAggregator.runSearchResultAggregator $ \cfg ->
          cfg & hideLogging
            & #kafkaConsumerCfgs . #publicTransportQuotes . #timeoutMilliseconds .~ kafkaConsumerTimeoutMilliseconds,
        DriverOfferBpp.runDriverOfferBpp $ \cfg ->
          cfg & hideLogging
            & #updateLocationRefreshPeriod .~ timeBetweenLocationUpdates
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
      Esq.migrateIfNeeded appBackendCfg.migrationPath True appBackendCfg.esqDBCfg
        >>= handleLeft exitDBMigrationFailure "Couldn't migrate app-backend database: "

      (transportCfg :: TransporterBackend.AppCfg) <- readDhallConfigDefault "beckn-transport"
      Esq.migrateIfNeeded transportCfg.migrationPath True transportCfg.esqDBCfg
        >>= handleLeft exitDBMigrationFailure "Couldn't migrate beckn-transporter database: "

      (driverOfferCfg :: DriverOfferBpp.AppCfg) <- readDhallConfigDefault "driver-offer-bpp"
      Esq.migrateIfNeeded driverOfferCfg.migrationPath True driverOfferCfg.esqDBCfg
        >>= handleLeft exitDBMigrationFailure "Couldn't migrate driver-offer BPP database: "

hideLogging :: HasField "loggerConfig" cfg LoggerConfig => cfg -> cfg
hideLogging cfg =
  cfg{loggerConfig =
        cfg.loggerConfig
          & #logToConsole .~ False
          & #logRawSql .~ False
          & #logToFile .~ True
     }
