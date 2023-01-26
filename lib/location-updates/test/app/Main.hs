module Main where

import API
import EulerHS.Prelude
import qualified "mock-google" Lib.IntegrationTests.Environment as Environment
import RedisAlgorithm
import Test.Tasty
import Test.Tasty.Hspec
import Utils

main :: IO ()
main = do
  wrapTests $ \appCfg -> specs appCfg >=> defaultMain

specs :: Environment.AppCfg -> AppEnv -> IO TestTree
specs appCfg appEnv = do
  googleCfgEncrypted <- Environment.buildGoogleConfig appCfg.encTools appCfg.googleCfg
  apiTreeSnap <- testSpec "Testing API using Snap-to-road" $ apiSpec appEnv googleCfgEncrypted
  apiTreeOsrm <- testSpec "Testing API using Osrm" $ apiSpec appEnv osrmConfig
  return $
    testGroup
      "Unit tests"
      [ locationUpdatesTree appEnv,
        apiTreeSnap,
        apiTreeOsrm
      ]
