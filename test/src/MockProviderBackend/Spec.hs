module MockProviderBackend.Spec where

import EulerHS.Prelude
import qualified MockProviderBackend.HealthCheck as HC
import qualified MockProviderBackend.Search as Search
import Test.Tasty.Hspec hiding (after)

mkTestTree :: IO TestTree
mkTestTree = do
  healthCheckSpec <- testSpec "HealthCheck" HC.spec
  searchSpec <- testSpec "Search" Search.spec
    testGroup
      "MockProviderBackend"
      [ healthCheckSpec,
        after AllSucceed "HealthCheck" $
          testGroup "APIs" [ searchSpec]
      ]
