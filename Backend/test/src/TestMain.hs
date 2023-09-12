{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedLabels #-}

module TestMain where

import qualified "beckn-gateway" App as Gateway
import qualified "driver-offer-allocator" App as ARDUAllocator
import qualified "driver-tracking-healthcheck" App as DriverHC
import qualified "dynamic-offer-driver-app" App as DriverOfferBpp
import qualified "mock-fcm" App as MockFcm
import qualified "mock-google" App as MockGoogle
import qualified "mock-public-transport-provider-platform" App as MockPublicTransportBpp
import qualified "mock-registry" App as MockRegistry
import qualified "mock-sms" App as MockSms
import qualified "public-transport-rider-platform" App as PublicTransport
import qualified "public-transport-search-consumer" App as PublicTransportSearchConsumer
import qualified "rider-app" App as AppBackend
import qualified "search-result-aggregator" App as SearchResultAggregator
import qualified Data.Text as T (replace, toUpper, unpack)
import EulerHS.Prelude
import qualified Kernel.External.Maps as Maps
import Kernel.Utils.Common hiding (id)
import qualified "mock-google" Lib.IntegrationTests.Environment as Environment
import qualified Mobility.ARDU.Spec as Mobility.ARDU
import qualified Mobility.ARDU.Utils as DriverOfferBppUtils
import qualified Mobility.AppBackend.Fixtures as Fixtures
import Mobility.AppBackend.Queries
import qualified Mobility.AppBackend.Utils as AppBackendUtils
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
      "rider-app",
      "beckn-gateway",
      "driver-tracking-healthcheck-service",
      "mock-registry",
      "public-transport-rider-platform",
      "mock-public-transport-provider-platform",
      "public-transport-search-consumer",
      "search-result-aggregator",
      "dynamic-offer-driver-app",
      "driver-offer-allocator",
      "mock-google"
    ]
  -- fetch google configs for using mock-google or real google
  testCfg <- Environment.readConfig ""
  googleCfgEncrypted <- Environment.buildGoogleConfig testCfg.encTools testCfg.googleCfg
  -- ... and run
  defaultMain =<< specs googleCfgEncrypted
  where
    setConfigEnv app = do
      Env.setEnv
        (T.unpack $ toEnvVar app <> "_CONFIG_PATH")
        (T.unpack $ "../dhall-configs/dev/" <> app <> ".dhall")
      Env.setEnv
        (T.unpack $ toEnvVar app <> "_MIGRATION_PATH")
        (T.unpack $ "../dev/migrations/" <> app)

    toEnvVar = T.toUpper . T.replace "-" "_"

specs :: Maps.MapsServiceConfig -> IO TestTree
specs googleCfg =
  specs'
    googleCfg
    [ Mobility.ARDU.mkTestTree,
      PublicTransport.mkTestTree
    ]

specs' :: Maps.MapsServiceConfig -> [IO TestTree] -> IO TestTree
specs' googleCfg trees = do
  readyTests <- sequence trees
  return $
    withResource
      (startServers >> onServersStarted)
      cleanupServers
      ( \_ ->
          testGroup
            "all"
            readyTests
      )
  where
    startServers = do
      {- We need to run some servers separately because due to internal PostgreSQL mechanism if you
         concurrently apply some queries to the same tables PostgreSQL fires pg_type_typname_nsp_index error.
         Therefore we need to run Driver Offer Allocator and Public Transport Search Consumer only after
         Driver Offer BPP and Public Trasport BAP applied their migrations since they apply migrations
         to the same tables.
         Details: https://www.postgresql.org/message-id/11235.1268149874%40sss.pgh.pa.us -}
      prepareTestResources
      threadDelaySec 1
      traverse_ forkIO firstWaveServers
      threadDelaySec 3
      traverse_ forkIO secondWaveServers
      -- Wait for servers to start up and migrations to run
      threadDelaySec 4

    onServersStarted = do
      runAppFlow "" $ do
        -- Esq.runTransaction $
        updateOrigAndDestRestriction Fixtures.yatriMerchantId ["Ernakulam", "Kochi", "Karnataka"] ["Kerala", "Kochi", "Karnataka"]

    cleanupServers _ = do
      AppBackendUtils.clearCachedMapsConfig
      DriverOfferBppUtils.clearCachedMapsConfig
      releaseTestResources
      signalProcess sigINT =<< getProcessID

    firstWaveServers =
      [ do
          DriverOfferBppUtils.changeCachedMapsConfig googleCfg
          DriverOfferBpp.runDynamicOfferDriverApp hideLogging,
        PublicTransport.runService hideLogging
      ]

    secondWaveServers =
      [ DriverHC.runDriverHealthcheck hideLogging,
        Gateway.runGateway hideLogging,
        do
          AppBackendUtils.changeCachedMapsConfig googleCfg
          AppBackend.runRiderApp $
            \cfg ->
              cfg & hideLogging,
        do
          MockSms.runMockSms hideLogging,
        MockFcm.runMockFcm hideLogging,
        MockRegistry.runRegistryService hideLogging,
        MockPublicTransportBpp.runMock $ \cfg ->
          cfg & #statusWaitTimeSec .~ mockWaitTimeSeconds
            & hideLogging,
        PublicTransportSearchConsumer.runPublicTransportSearchConsumer $ \cfg ->
          cfg & hideLogging
            & #kafkaConsumerCfgs . #publicTransportSearch . #timeoutMilliseconds .~ kafkaConsumerTimeoutMilliseconds,
        SearchResultAggregator.runSearchResultAggregator $ \cfg ->
          cfg & hideLogging
            & #kafkaConsumerCfgs . #publicTransportQuotes . #timeoutMilliseconds .~ kafkaConsumerTimeoutMilliseconds,
        ARDUAllocator.runDriverOfferAllocator $ \cfg ->
          cfg{appCfg = cfg.appCfg & hideLogging,
              schedulerConfig = cfg.schedulerConfig & hideLogging
             },
        MockGoogle.runService $ \cfg ->
          cfg & hideLogging
            & #mockDataPath .~ "../app/mocks/google/mock-data/"
      ]

hideLogging :: HasField "loggerConfig" cfg LoggerConfig => cfg -> cfg
hideLogging cfg =
  cfg{loggerConfig =
        cfg.loggerConfig
          & #logToConsole .~ False
          & #logRawSql .~ False
          & #logToFile .~ True
     }
