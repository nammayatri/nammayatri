{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module App.Types where

import qualified Data.Text as T
import EulerHS.Prelude
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.App
import Kernel.Types.Common hiding (id)
import Kernel.Types.Flow
import Kernel.Utils.App (lookupDeploymentVersion)
import Kernel.Utils.IOLogging
import Kernel.Utils.Servant.Client (HttpClientOptions, RetryCfg)
import Kernel.Utils.Shutdown
import System.Environment (lookupEnv)
import Types.Common

data AppCfg = AppCfg
  { port :: Int,
    loggerConfig :: LoggerConfig,
    callbackWaitTimeMilliSec :: Milliseconds,
    httpClientOptions :: HttpClientOptions,
    shortDurationRetryCfg :: RetryCfg,
    longDurationRetryCfg :: RetryCfg,
    graceTerminationPeriod :: Seconds,
    webhookUrl :: BaseUrl,
    secret :: Text,
    accountId :: AccountId,
    apiKey :: ApiKey,
    criticalAPIs :: ApiPriorityList
  }
  deriving (Generic)

data AppEnv = AppEnv
  { port :: Int,
    loggerConfig :: LoggerConfig,
    loggerEnv :: LoggerEnv,
    callbackWaitTimeMilliSec :: Milliseconds,
    graceTerminationPeriod :: Seconds,
    coreMetrics :: CoreMetricsContainer,
    isShuttingDown :: Shutdown,
    webhookUrl :: BaseUrl,
    secret :: Text,
    httpClientOptions :: HttpClientOptions,
    shortDurationRetryCfg :: RetryCfg,
    longDurationRetryCfg :: RetryCfg,
    accountId :: AccountId,
    apiKey :: ApiKey,
    version :: DeploymentVersion,
    criticalAPIs :: ApiPriorityList
  }
  deriving (Generic)

buildAppEnv :: AppCfg -> IO AppEnv
buildAppEnv AppCfg {..} = do
  hostname <- map T.pack <$> lookupEnv "POD_NAME"
  version <- lookupDeploymentVersion
  loggerEnv <- prepareLoggerEnv loggerConfig hostname
  coreMetrics <- registerCoreMetricsContainer
  isShuttingDown <- mkShutdown
  return $ AppEnv {..}

releaseAppEnv :: AppEnv -> IO ()
releaseAppEnv AppEnv {..} =
  releaseLoggerEnv loggerEnv

type FlowHandler = FlowHandlerR AppEnv

type FlowServer api = FlowServerR AppEnv api

type Flow = FlowR AppEnv
