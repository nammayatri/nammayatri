module Main where

import API
import EulerHS.Prelude
import RedisAlgorithm
import Test.Tasty
import Test.Tasty.Hspec
import Utils

main :: IO ()
main = do
  wrapTests (specs >=> defaultMain)

specs :: AppEnv -> IO TestTree
specs appEnv = do
  apiTreeSnap <- testSpec "Testing API using Snap-to-road" $ apiSpec appEnv googleConfig
  apiTreeOsrm <- testSpec "Testing API using Osrm" $ apiSpec appEnv osrmConfig
  return $
    testGroup
      "Unit tests"
      [ locationUpdatesTree appEnv,
        apiTreeSnap,
        apiTreeOsrm
      ]
