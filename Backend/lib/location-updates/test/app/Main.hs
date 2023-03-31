{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedLabels #-}

module Main where

import API
import Control.Lens ((.~))
import qualified "mock-google" App as MockGoogle
import EulerHS.Prelude hiding ((.~))
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
          & #logRawSql .~ True
          & #logToFile .~ True
     }
