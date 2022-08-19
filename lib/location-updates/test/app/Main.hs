module Main where

import API
import EulerHS.Prelude
import RedisAlgorithm
import Test.Tasty
import Test.Tasty.Hspec
import Types

main :: IO ()
main = do
  wrapTests (specs >=> defaultMain)

specs :: AppEnv -> IO TestTree
specs appEnv = do
  apiTree <- testSpec "Testing API" $ apiSpec appEnv
  return $
    testGroup
      "Unit tests"
      [ locationUpdatesTree appEnv,
        apiTree
      ]
