module Main where

import qualified Data.Text as T (replace, toUpper, unpack)
import EulerHS.Prelude
import qualified Mobility.Fixtures as Mobility
import qualified Mobility.Spec as Mobility
import qualified MockAppBackend.Fixtures as MockAppBackend
import qualified MockAppBackend.Spec as MockAppBackend
import qualified MockProviderBackend.Fixtures as MockProviderBackend
import qualified MockProviderBackend.Spec as MockProviderBackend
import System.Environment (setEnv)
import Test.Tasty

main :: IO ()
main = do
  mapM_
    setConfigEnv
    [ "app-backend",
      "beckn-transport",
      "beckn-gateway",
      "mock-app-backend",
      "mock-provider-backend"
    ]
  defaultMain =<< specs
  where
    setConfigEnv app =
      setEnv
        (T.unpack $ toEnvVar app <> "_CONFIG_PATH")
        (T.unpack $ "../dev/config/" <> app <> ".dhall")
    toEnvVar = T.toUpper . T.replace "-" "_"

specs :: IO TestTree
specs = do
  mobilityTests <- Mobility.mkTestTree
  mockAppBackendTests <- MockAppBackend.mkTestTree
  mockProviderBackendTests <- MockProviderBackend.mkTestTree
  return $
    withResource
      startServers
      cleanupServers
      ( \_ ->
          testGroup
            "all"
            [ mockAppBackendTests,
              mockProviderBackendTests,
              mobilityTests
            ]
      )
  where
    startServers = do
      (appTid, tbeTid, gatewayTid) <- Mobility.startServers
      mockAppTid <- MockAppBackend.startServer
      mockProviderTid <- MockProviderBackend.startServer
      return [appTid, tbeTid, gatewayTid, mockAppTid, mockProviderTid]
    cleanupServers = traverse_ killThread
