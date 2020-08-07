module Main where

import EulerHS.Prelude
import qualified Mobility.Fixtures as Mobility
import qualified Mobility.Spec as Mobility
import qualified MockAppBackend.Fixtures as MockAppBackend
import qualified MockAppBackend.Spec as MockAppBackend
import qualified MockProviderBackend.Fixtures as MockProviderBackend
import Test.Tasty

main :: IO ()
main = defaultMain =<< specs

specs :: IO TestTree
specs = do
  mobilityTests <- Mobility.mkTestTree
  mockAppBackendTests <- MockAppBackend.mkTestTree
  return $
    withResource
      startServers
      (\_ -> return ())
      ( \_ ->
          testGroup
            "all"
            [ mockAppBackendTests,
              mobilityTests
            ]
      )
  where
    startServers = do
      (appTid, tbeTid, gatewayTid) <- Mobility.startServers
      mockAppTid <- MockAppBackend.startServer
      mockProviderTid <- MockProviderBackend.startServer
      return (appTid, tbeTid, gatewayTid, mockAppTid, mockProviderTid)
