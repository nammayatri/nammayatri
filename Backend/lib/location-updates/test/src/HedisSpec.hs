module HedisSpec where

import EulerHS.Prelude
import qualified Kernel.Storage.Hedis.Queries as Hedis
import Test.Tasty
import Test.Tasty.HUnit
import Utils

hedisTests :: AppEnv -> TestTree
hedisTests appEnv =
  testGroup
    "Hedis edge cases"
    [ testCase "hmGet with empty fields returns empty list" $
        runFlow "hmGet-empty-fields" appEnv $ do
          result <- Hedis.hmGet "nonexistent-key" ([] :: [Text])
          liftIO $ result @?= ([] :: [Maybe Text]),
      testCase "hmGet with valid key and missing fields returns Nothings" $
        runFlow "hmGet-missing-fields" appEnv $ do
          result <- Hedis.hmGet "nonexistent-key" ["field1", "field2"]
          liftIO $ result @?= ([Nothing, Nothing] :: [Maybe Text])
    ]
