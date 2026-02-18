{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Environment where

import Kernel.Prelude
import Kernel.Streaming.Kafka.Producer.Types (KafkaProducerTools)
import Kernel.Types.Common
import Kernel.Types.Flow
import Kernel.Utils.App (getPodName, lookupDeploymentVersion)
import Kernel.Utils.Common
import Kernel.Utils.Dhall (FromDhall)
import Kernel.Utils.IOLogging
import Kernel.Utils.Shutdown
import Lib.GoogleConfig (GoogleCfgUnencrypted)
import System.Environment (lookupEnv)
import Tools.Metrics

data AppCfg = AppCfg
  { port :: Int,
    loggerConfig :: LoggerConfig,
    graceTerminationPeriod :: Seconds,
    mockDataPath :: FilePath,
    googleCfg :: Maybe GoogleCfgUnencrypted,
    snapToRoadIdentityMode :: Bool
  }
  deriving (Generic, FromDhall)

data AppEnv = AppEnv
  { port :: Int,
    loggerConfig :: LoggerConfig,
    graceTerminationPeriod :: Seconds,
    isShuttingDown :: Shutdown,
    loggerEnv :: LoggerEnv,
    coreMetrics :: CoreMetricsContainer,
    mockDataPath :: FilePath,
    googleCfg :: Maybe GoogleCfgUnencrypted,
    version :: DeploymentVersion,
    snapToRoadIdentityMode :: Bool,
    requestId :: Maybe Text,
    shouldLogRequestId :: Bool,
    sessionId :: Maybe Text,
txnId :: Maybe Text,
    kafkaProducerForART :: Maybe KafkaProducerTools,
    url :: Maybe Text
  }
  deriving (Generic)

type MockDataFlow m r = HasFlowEnv m r '["mockDataPath" ::: FilePath]

buildAppEnv :: AppCfg -> IO AppEnv
buildAppEnv AppCfg {..} = do
  podName <- getPodName
  version <- lookupDeploymentVersion
  loggerEnv <- prepareLoggerEnv loggerConfig podName
  coreMetrics <- registerCoreMetricsContainer
  let requestId = Nothing
  shouldLogRequestId <- fromMaybe False . (>>= readMaybe) <$> lookupEnv "SHOULD_LOG_REQUEST_ID"
  let sessionId = Nothing
      txnId = Nothing
  let kafkaProducerForART = Nothing
  isShuttingDown <- mkShutdown
  let url = Nothing
  return $ AppEnv {..}

releaseAppEnv :: AppEnv -> IO ()
releaseAppEnv AppEnv {..} =
  releaseLoggerEnv loggerEnv

type FlowHandler = FlowHandlerR AppEnv

type FlowServer api = FlowServerR AppEnv api

type Flow = FlowR AppEnv
