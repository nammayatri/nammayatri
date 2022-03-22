module PublicTransport.Spec where

import Beckn.Prelude
import qualified PublicTransport.HealthCheck as HC
import qualified PublicTransport.Search as Search
import Test.Tasty
import Test.Tasty.Hspec hiding (after)

mkTestTree :: IO TestTree
mkTestTree = do
  hcSpec <- testSpec "HealthCheck" HC.spec
  searchSpec <- testSpec "Search" Search.spec
  pure $
    testGroup
      "PublicTransport"
      [ hcSpec,
        after AllSucceed "HealthCheck" $
          testGroup "APIs" [searchSpec]
      ]
