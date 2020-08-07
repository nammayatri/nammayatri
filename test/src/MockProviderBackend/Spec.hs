module MockProviderBackend.Spec where

import EulerHS.Prelude
import qualified MockProviderBackend.HealthCheck as HC
import qualified MockProviderBackend.Search as Search
import qualified MockProviderBackend.Select as Select
import Test.Tasty
import Test.Tasty.Hspec hiding (after)

mkTestTree :: IO TestTree
mkTestTree = do
  healthCheckSpec <- testSpec "HealthCheck" HC.spec
  searchSpec <- testSpec "Search" Search.spec
  selectSpec <- testSpec "Select" Select.spec
  return $
    testGroup
      "MockProviderBackend"
      [ healthCheckSpec,
        after AllSucceed "HealthCheck" $
          testGroup "APIs" [searchSpec, selectSpec]
      ]
