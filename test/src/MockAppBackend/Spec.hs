module MockAppBackend.Spec where

import EulerHS.Prelude
import MockAppBackend.Fixtures (startServers)
import qualified MockAppBackend.HealthCheck as HC
import qualified MockAppBackend.OnConfirm as OnConfirm
import qualified MockAppBackend.OnInit as OnInit
import qualified MockAppBackend.OnSearch as OnSearch
import qualified MockAppBackend.OnSelect as OnSelect
import qualified MockAppBackend.TriggerSearch as TriggerSearch
import Test.Tasty
import Test.Tasty.Hspec hiding (after)

mkTestTree :: IO TestTree
mkTestTree = do
  healthCheckSpec <- testSpec "HealthCheck" HC.spec
  triggerSearchSpec <- testSpec "TriggerSearch" TriggerSearch.spec
  onSearchSpec <- testSpec "OnSearch" OnSearch.spec
  onSelectSpec <- testSpec "OnSelect" OnSelect.spec
  onInitSpec <- testSpec "OnInit" OnInit.spec
  onConfirmSpec <- testSpec "OnConfirm" OnConfirm.spec
  return $
    withResource
      startServers
      (\(mockAppTid, mockProviderTid, gatewayTid) -> traverse_ killThread [mockAppTid, mockProviderTid, gatewayTid])
      ( \_ ->
          testGroup
            "MockAppBackend"
            [ healthCheckSpec,
              after AllSucceed "HealthCheck" $
                testGroup "APIs" [triggerSearchSpec, onSearchSpec, onSelectSpec, onInitSpec, onConfirmSpec]
            ]
      )
