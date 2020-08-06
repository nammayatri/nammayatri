module MockAppBackend.Spec where

import EulerHS.Prelude
import MockAppBackend.Fixtures (startServers)
import qualified MockAppBackend.HealthCheck as HC
import qualified MockAppBackend.TriggerSearch as TriggerSearch
import Test.Tasty
import Test.Tasty.Hspec hiding (after)

mkTestTree :: IO TestTree
mkTestTree = do
  healthCheckSpec <- testSpec "HealthCheck" HC.spec
  triggerSearchSpec <- testSpec "TriggerSearch" TriggerSearch.spec

  return $
    withResource
      startServers
      (\(mockAppTid, mockProviderTid, gatewayTid) -> traverse_ killThread [mockAppTid, mockProviderTid, gatewayTid])
      ( \_ ->
          testGroup
            "MockAppBackend"
            [ healthCheckSpec,
              after AllSucceed "HealthCheck" $
                testGroup "APIs" [triggerSearchSpec]
            ]
      )
