module FmdWrapper.Spec where

import EulerHS.Prelude
import FmdWrapper.Flow.Confirm as Confirm
import FmdWrapper.Flow.Init as Init
import FmdWrapper.Flow.Search as Search
import FmdWrapper.HealthCheck as HC
import Test.Tasty
import Test.Tasty.Hspec hiding (after)

mkTestTree :: IO TestTree
mkTestTree = do
  hcSpec <- testSpec "HealthCheck" HC.spec
  searchSpec <- testSpec "Search" Search.spec
  initSpec <- testSpec "Init" Init.spec
  confirmSpec <- testSpec "Confirm" Confirm.spec
  return $
    testGroup
      "Fmd"
      [ hcSpec,
        after AllSucceed "HealthCheck" $
          testGroup "APIs" [searchSpec, initSpec, confirmSpec]
      ]
