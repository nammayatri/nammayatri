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
import Kernel.Storage.Hedis (HedisCfg, HedisEnv, connectHedisCluster, disconnectHedis)
import Kernel.Streaming.Kafka.Producer.Types
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.CacheFlow
import Kernel.Types.Common
import Kernel.Types.Flow
import Kernel.Types.SlidingWindowLimiter
import Kernel.Utils.App (getPodName, lookupDeploymentVersion)
import Kernel.Utils.Dhall (FromDhall)
import Kernel.Utils.IOLogging
import Kernel.Utils.Shutdown
import qualified System.Environment as SE

data DriverAppConfig = DriverAppConfig
  { url :: BaseUrl,
    apiKey :: Text
  }
  deriving (Generic, FromDhall)

data AppCfg = AppCfg
  { port :: Int,
    hedisClusterCfg :: HedisCfg,
    loggerConfig :: LoggerConfig,
    graceTerminationPeriod :: Seconds,
    kafkaProducerCfg :: KafkaProducerCfg,
    secondaryKafkaProducerCfg :: Maybe KafkaProducerCfg,
    riderSDKEventsKafkaTopic :: Text,
    driverSDKEventsKafkaTopic :: Text,
    metroWebviewEventsKafkaTopic :: Text,
    apiRateLimitOptions :: APIRateLimitOptions,
    driverAppConfig :: DriverAppConfig,
    cacheConfig :: CacheConfig
  }
  deriving (Generic, FromDhall)

data AppEnv = AppEnv
  { port :: Int,
    hedisEnv :: HedisEnv,
    hedisNonCriticalEnv :: HedisEnv,
    hedisClusterEnv :: HedisEnv,
    hedisNonCriticalClusterEnv :: HedisEnv,
    loggerConfig :: LoggerConfig,
    graceTerminationPeriod :: Seconds,
    isShuttingDown :: Shutdown,
    kafkaProducerCfg :: KafkaProducerCfg,
    kafkaProducerTools :: KafkaProducerTools,
    riderSDKEventsKafkaTopic :: Text,
    driverSDKEventsKafkaTopic :: Text,
    metroWebviewEventsKafkaTopic :: Text,
    loggerEnv :: LoggerEnv,
    version :: DeploymentVersion,
    coreMetrics :: CoreMetricsContainer,
    apiRateLimitOptions :: APIRateLimitOptions,
    driverAppConfig :: DriverAppConfig,
    hedisMigrationStage :: Bool,
    enablePrometheusMetricLogging :: Bool,
    enableRedisLatencyLogging :: Bool,
    cacheConfig :: CacheConfig,
    requestId :: Maybe Text,
    shouldLogRequestId :: Bool,
    sessionId :: Maybe Text,
    kafkaProducerForART :: Maybe KafkaProducerTools,
    url :: Maybe Text
  }
  deriving (Generic)

buildAppEnv :: AppCfg -> IO AppEnv
buildAppEnv AppCfg {..} = do
  podName <- getPodName
  loggerEnv <- prepareLoggerEnv loggerConfig podName
  kafkaProducerTools <- buildKafkaProducerTools kafkaProducerCfg secondaryKafkaProducerCfg
  coreMetrics <- registerCoreMetricsContainer
  version <- lookupDeploymentVersion
  isShuttingDown <- mkShutdown
  let requestId = Nothing
  let sessionId = Nothing
  shouldLogRequestId <- fromMaybe False . (>>= readMaybe) <$> SE.lookupEnv "SHOULD_LOG_REQUEST_ID"
  let kafkaProducerForART = Just kafkaProducerTools
  let modifierFunc = ("sdk-event:" <>)
  hedisEnv <- connectHedisCluster hedisClusterCfg modifierFunc
  hedisClusterEnv <- connectHedisCluster hedisClusterCfg modifierFunc
  hedisNonCriticalEnv <- connectHedisCluster hedisClusterCfg modifierFunc
  hedisNonCriticalClusterEnv <- connectHedisCluster hedisClusterCfg modifierFunc
  let hedisMigrationStage = False
      enablePrometheusMetricLogging = False
      enableRedisLatencyLogging = False
  let url = Nothing
  return $ AppEnv {..}

releaseAppEnv :: AppEnv -> IO ()
releaseAppEnv AppEnv {..} = do
  releaseKafkaProducerTools kafkaProducerTools
  releaseLoggerEnv loggerEnv
  disconnectHedis hedisClusterEnv

type ApiKey = Text

type Env = EnvR AppEnv

type FlowHandler = FlowHandlerR AppEnv

type FlowServer api = FlowServerR AppEnv api

type Flow = FlowR AppEnv
