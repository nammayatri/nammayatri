{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module TestMain where

import qualified "app-backend" App as AppBackend
import qualified "beckn-gateway" App as Gateway
import qualified "beckn-transport" App as TransporterBackend
import qualified "beckn-transport-allocator" App as YatriAllocator
import qualified "beckn-transport-driver-tracking-health-check" App as DriverHC
import qualified "driver-offer-allocator" App as ARDUAllocator
import qualified "driver-offer-bpp" App as DriverOfferBpp
import qualified "mock-fcm" App as MockFcm
import qualified "mock-public-transport-bpp" App as MockPublicTransportBpp
import qualified "mock-registry" App as MockRegistry
import qualified "mock-sms" App as MockSms
import qualified "public-transport-bap" App as PublicTransport
import qualified "public-transport-search-consumer" App as PublicTransportSearchConsumer
import qualified "search-result-aggregator" App as SearchResultAggregator
import Beckn.Exit (exitDBMigrationFailure)
import qualified Beckn.Storage.Esqueleto as Esq
import qualified Beckn.Storage.Esqueleto.Migration as Esq
import Beckn.Utils.App (handleLeft)
import Beckn.Utils.Common hiding (id)
import Beckn.Utils.Dhall (readDhallConfigDefault)
import qualified Data.Text as T (replace, toUpper, unpack)
import qualified "app-backend" Environment as AppBackend
import qualified "beckn-transport" Environment as TransporterBackend
import qualified "driver-offer-bpp" Environment as DriverOfferBpp
import EulerHS.Prelude
import qualified Mobility.ARDU.Spec as Mobility.ARDU
import Mobility.AppBackend.Queries
import qualified Mobility.Transporter.Spec as Transporter.Mobility
import PublicTransport.Common
import qualified PublicTransport.Spec as PublicTransport
import Resources
import System.Environment as Env (setEnv)
import System.Posix
import Test.Tasty
import TestSilentIOLogger ()
import Utils (runAppFlow)

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
      "driver-offer-bpp",
      "driver-offer-allocator"
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
    [ Transporter.Mobility.mkTestTree,
      Mobility.ARDU.mkTestTree,
      PublicTransport.mkTestTree
    ]

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
      [ YatriAllocator.runAllocator \cfg ->
          cfg & hideLogging
            & #driverNotificationExpiry .~ 18,
        DriverHC.runDriverHealthcheck hideLogging,
        Gateway.runGateway hideLogging,
        do
          runAppFlow "" $ Esq.runTransaction $ updateOrigAndDestRestriction ["Ernakulam", "Kochi", "Karnataka"] ["Kerala", "Kochi", "Karnataka"]
          AppBackend.runAppBackend $
            \cfg ->
              cfg & hideLogging,
        TransporterBackend.runTransporterBackendApp $
          \cfg ->
            cfg & hideLogging,
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
        DriverOfferBpp.runDriverOfferBpp $
          \cfg ->
            cfg & hideLogging,
        ARDUAllocator.runDriverOfferAllocator $ \cfg ->
          cfg{appCfg = cfg.appCfg & hideLogging,
              schedulerConfig = cfg.schedulerConfig & hideLogging
             }
      ]

    startServers servers = do
      migrateDB
      prepareTestResources
      threadDelaySec 1
      traverse_ forkIO servers
      -- Wait for servers to start up and migrations to run
      threadDelaySec 4

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
