module MockProviderBackend.Spec where

import EulerHS.Prelude
import qualified MockProviderBackend.Confirm as Confirm
import qualified MockProviderBackend.HealthCheck as HC
import qualified MockProviderBackend.Init as Init
import qualified MockProviderBackend.Search as Search
import qualified MockProviderBackend.Select as Select
import qualified MockProviderBackend.Update as Update
import Test.Tasty
import Test.Tasty.Hspec hiding (after)

mkTestTree :: IO TestTree
mkTestTree = do
  healthCheckSpec <- testSpec "HealthCheck" HC.spec
  searchSpec <- testSpec "Search" Search.spec
  selectSpec <- testSpec "Select" Select.spec
  confirmSpec <- testSpec "Confirm" Confirm.spec
  initSpec <- testSpec "Init" Init.spec
  updateSpec <- testSpec "Update" Update.spec
  return $
    testGroup
      "MockProviderBackend"
      [ healthCheckSpec,
        after AllSucceed "HealthCheck" $
          testGroup "APIs" [searchSpec, selectSpec, confirmSpec, initSpec, updateSpec]
      ]
