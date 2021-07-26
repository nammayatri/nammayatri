module FmdWrapper.Spec where

import EulerHS.Prelude
import qualified FmdWrapper.Flow.CompleteFlow as CompleteFlow
import qualified FmdWrapper.Flow.Confirm as Confirm
import qualified FmdWrapper.Flow.Search as Search
import qualified FmdWrapper.HealthCheck as HC
import Test.Tasty
import Test.Tasty.Hspec hiding (after)

mkTestTree :: IO TestTree
mkTestTree = do
  hcSpec <- testSpec "HealthCheck" HC.spec
  searchSpec <- testSpec "Search" Search.spec
  confirmSpec <- testSpec "Confirm" Confirm.spec
  completeFlowSpec <- testSpec "Complete flow" CompleteFlow.spec
  return $
    testGroup
      "Fmd"
      [ hcSpec,
        after AllSucceed "HealthCheck" $
          testGroup "APIs" [searchSpec, confirmSpec, completeFlowSpec]
      ]
