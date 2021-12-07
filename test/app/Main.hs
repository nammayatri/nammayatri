{-# LANGUAGE OverloadedLabels #-}

module Main where

import qualified "app-backend" App as AppBackend
import qualified "beckn-gateway" App as Gateway
import qualified "beckn-transport" App as TransporterBackend
import qualified "fmd-wrapper" App as FmdWrapper
import qualified "mock-fcm" App as MockFcm
import qualified "mock-registry" App as MockRegistry
import qualified "mock-sms" App as MockSms
import qualified "beckn-transport" BackgroundTaskManager as TransporterBGTM
import qualified Data.Text as T (replace, toUpper, unpack)
import EulerHS.Prelude
import qualified FmdWrapper.Spec as FmdWrapper
import qualified Mobility.Spec as Mobility
import System.Environment (setEnv)
import Test.Tasty
import "app-backend" Types.Geofencing

main :: IO ()
main = do
  -- We can't really spawn off multiple instances of our servers, so serialise...
  setEnv "TASTY_NUM_THREADS" "1"
  -- Set some config paths in environment...
  mapM_
    setConfigEnv
    [ "app-backend",
      "beckn-transport",
      "beckn-transport-btm",
      "beckn-gateway",
      "fmd-wrapper",
      "mock-registry"
    ]
  -- ... and run
  defaultMain =<< specs
  where
    setConfigEnv app = do
      setEnv
        (T.unpack $ toEnvVar app <> "_CONFIG_PATH")
        (T.unpack $ "../dhall-configs/dev/" <> app <> ".dhall")
      setEnv
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
      [ TransporterBGTM.runBackgroundTaskManager $ \cfg ->
          cfg & #appCfg . #loggerConfig . #logToConsole .~ False
            & #appCfg . #loggerConfig . #logRawSql .~ False
            & #driverAllocationConfig . #driverNotificationExpiry .~ 18
            & #driverAllocationConfig . #rideAllocationExpiry .~ 18,
        Gateway.runGateway $ \cfg ->
          cfg & #loggerConfig . #logToConsole .~ False
            & #loggerConfig . #logRawSql .~ False,
        AppBackend.runAppBackend $
          \cfg ->
            cfg & #loggerConfig . #logToConsole .~ False
              & #loggerConfig . #logRawSql .~ False
              & #geofencingConfig . #origin .~ Region "Ernakulam"
              & #geofencingConfig . #destination .~ Region "Kerala",
        TransporterBackend.runTransporterBackendApp $ \cfg ->
          cfg & #loggerConfig . #logToConsole .~ False
            & #loggerConfig . #logRawSql .~ False,
        FmdWrapper.runFMDWrapper $ \cfg ->
          cfg & #loggerConfig . #logToConsole .~ False
            & #loggerConfig . #logRawSql .~ False,
        MockSms.runMockSms $
          (#loggerConfig . #logToConsole .~ False)
            . (#loggerConfig . #logRawSql .~ False),
        MockFcm.runMockFcm $
          (#loggerConfig . #logToConsole .~ False)
            . (#loggerConfig . #logRawSql .~ False),
        MockRegistry.runRegistryService $
          (#loggerConfig . #logToConsole .~ False)
            . (#loggerConfig . #logRawSql .~ False)
      ]

    startServers servers = do
      threadIds <- traverse forkIO servers
      -- Wait for servers to start up and migrations to run
      threadDelay 5e6
      return threadIds

    cleanupServers = traverse_ killThread
