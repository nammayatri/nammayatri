{-# LANGUAGE OverloadedLabels #-}

module Main where

import API
import qualified "mock-google" App as MockGoogle
import EulerHS.Prelude
import Kernel.Utils.Common
import qualified "mock-google" Lib.IntegrationTests.Environment as Environment
import RedisAlgorithm
import System.Environment as Env (setEnv)
import Test.Tasty
import Test.Tasty.Hspec
import Utils

main :: IO ()
main = do
  wrapTests $ \appCfg -> specs appCfg >=> defaultMain

specs :: Environment.AppCfg -> AppEnv -> IO TestTree
specs appCfg appEnv = do
  googleCfgEncrypted <- Environment.buildGoogleConfig appCfg.encTools appCfg.googleCfg
  -- Try to run mock-google. We already run it in integration tests, but we don't know which tests will start faster
  Env.setEnv "MOCK_GOOGLE_CONFIG_PATH" "../../dhall-configs/dev/mock-google.dhall"
  _ <- forkIO $
    MockGoogle.runService $ \cfg ->
      cfg & hideLogging
        & #mockDataPath .~ "../../app/mocks/google/mock-data/"
  apiTreeSnap <- testSpec "Testing API using Snap-to-road" $ apiSpec appEnv googleCfgEncrypted
  apiTreeOsrm <- testSpec "Testing API using Osrm" $ apiSpec appEnv osrmConfig
  return $
    testGroup
      "Unit tests"
      [ locationUpdatesTree appEnv,
        apiTreeSnap,
        apiTreeOsrm
      ]

hideLogging :: HasField "loggerConfig" cfg LoggerConfig => cfg -> cfg
hideLogging cfg =
  cfg{loggerConfig =
        cfg.loggerConfig
          & #logToConsole .~ False
          & #logRawSql .~ False
          & #logToFile .~ True
     }
