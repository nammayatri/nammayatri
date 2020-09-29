module Main where

import qualified "app-backend" App as AppBackend
import qualified "beckn-gateway" App as Gateway
import qualified "beckn-transport" App as TransporterBackend
import qualified "fmd-wrapper" App as FmdWrapper
import qualified "mock-app-backend" App as MockAppBackend
import qualified "mock-provider-backend" App as MockProviderBackend
import qualified Data.Text as T (replace, toUpper, unpack)
import EulerHS.Prelude
import qualified FmdWrapper.Spec as FmdWrapper
import qualified Mobility.Spec as Mobility
import qualified MockAppBackend.Spec as MockAppBackend
import qualified MockProviderBackend.Spec as MockProviderBackend
import System.Environment (setEnv)
import Test.Tasty

main :: IO ()
main = do
  -- We can't really spawn off multiple instances of our servers, so serialise...
  setEnv "TASTY_NUM_THREADS" "1"
  -- Set some config paths in environment...
  mapM_
    setConfigEnv
    [ "app-backend",
      "beckn-transport",
      "beckn-gateway",
      "mock-app-backend",
      "mock-provider-backend",
      "fmd-wrapper"
    ]
  -- ... and run
  defaultMain =<< specs
  where
    setConfigEnv app = do
      setEnv
        (T.unpack $ toEnvVar app <> "_CONFIG_PATH")
        (T.unpack $ "../dev/config/" <> app <> ".dhall")
      setEnv
        (T.unpack $ toEnvVar app <> "_MIGRATION_PATH")
        (T.unpack $ "../dev/migrations/" <> app)

    toEnvVar = T.toUpper . T.replace "-" "_"

specs :: IO TestTree
specs = do
  mobilityTests <- Mobility.mkTestTree
  mockAppBackendTests <- MockAppBackend.mkTestTree
  mockProviderBackendTests <- MockProviderBackend.mkTestTree
  fmdTests <- FmdWrapper.mkTestTree

  return $
    withResource
      (startServers allServers)
      cleanupServers
      ( \_ ->
          testGroup
            "all"
            [ mobilityTests,
              mockAppBackendTests,
              mockProviderBackendTests,
              fmdTests
            ]
      )
  where
    allServers =
      [ Gateway.runGateway True,
        AppBackend.runAppBackend True,
        TransporterBackend.runTransporterBackendApp True,
        FmdWrapper.runFMDWrapper True,
        MockAppBackend.runMockApp True,
        MockProviderBackend.runMockProvider True
      ]

    startServers servers = do
      threadIds <- traverse forkIO servers
      -- Wait for servers to start up and migrations to run
      threadDelay 1e6
      return threadIds

    cleanupServers = traverse_ killThread
