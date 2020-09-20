module FmdWrapper.Spec where

import EulerHS.Prelude
import FmdWrapper.HealthCheck as HC
import FmdWrapper.Search as Search
import Test.Tasty
import Test.Tasty.Hspec hiding (after)

mkTestTree :: IO TestTree
mkTestTree = do
  hcSpec <- testSpec "HealthCheck" HC.spec
  searchSpec <- testSpec "Search" Search.spec
  return $
    testGroup
      "Fmd"
      [ hcSpec,
        after AllSucceed "HealthCheck" $
          testGroup "APIs" [searchSpec]
      ]
