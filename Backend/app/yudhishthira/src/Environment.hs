{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Environment where

import Kernel.External.Encryption (EncTools)
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config
import Kernel.Storage.Hedis (HedisCfg, HedisEnv, connectHedis, connectHedisCluster, disconnectHedis)
import Kernel.Streaming.Kafka.Producer.Types
import Kernel.Tools.Metrics.CoreMetrics
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Tools.Slack.Internal
import Kernel.Types.CacheFlow
import Kernel.Types.Common
import Kernel.Types.Flow
import Kernel.Types.SlidingWindowLimiter
import Kernel.Utils.App (getPodName, lookupDeploymentVersion)
import Kernel.Utils.Dhall (FromDhall)
import Kernel.Utils.IOLogging
import Kernel.Utils.Servant.Client
import Kernel.Utils.Shutdown
import System.Environment

data AppCfg = AppCfg
  { esqDBCfg :: EsqDBConfig,
    esqDBReplicaCfg :: EsqDBConfig,
    hedisCfg :: HedisCfg,
    hedisClusterCfg :: HedisCfg,
    hedisNonCriticalCfg :: HedisCfg,
    hedisNonCriticalClusterCfg :: HedisCfg,
    hedisMigrationStage :: Bool,
    cutOffHedisCluster :: Bool,
    port :: Int,
    migrationPath :: [FilePath],
    autoMigrate :: Bool,
    loggerConfig :: LoggerConfig,
    graceTerminationPeriod :: Seconds,
    apiRateLimitOptions :: APIRateLimitOptions,
    shareRideApiRateLimitOptions :: APIRateLimitOptions,
    httpClientOptions :: HttpClientOptions,
    shortDurationRetryCfg :: RetryCfg,
    longDurationRetryCfg :: RetryCfg,
    authTokenCacheExpiry :: Seconds,
    encTools :: EncTools,
    enableRedisLatencyLogging :: Bool,
    enablePrometheusMetricLogging :: Bool,
    cacheConfig :: CacheConfig,
    cacConfig :: CacConfig,
    slackToken :: Text,
    slackChannel :: Text,
    apiToken :: Text,
    kafkaProducerCfg :: KafkaProducerCfg,
    kvConfigUpdateFrequency :: Int
  }
  deriving (Generic, FromDhall)

data AppEnv = AppEnv
  { esqDBEnv :: EsqDBEnv,
    esqDBReplicaEnv :: EsqDBEnv,
    hedisEnv :: HedisEnv,
    hedisNonCriticalEnv :: HedisEnv,
    hedisNonCriticalClusterEnv :: HedisEnv,
    hedisClusterEnv :: HedisEnv,
    hedisMigrationStage :: Bool,
    cutOffHedisCluster :: Bool,
    port :: Int,
    loggerConfig :: LoggerConfig,
    loggerEnv :: LoggerEnv,
    graceTerminationPeriod :: Seconds,
    apiRateLimitOptions :: APIRateLimitOptions,
    shareRideApiRateLimitOptions :: APIRateLimitOptions,
    httpClientOptions :: HttpClientOptions,
    shortDurationRetryCfg :: RetryCfg,
    longDurationRetryCfg :: RetryCfg,
    authTokenCacheExpiry :: Seconds,
    encTools :: EncTools,
    coreMetrics :: Metrics.CoreMetricsContainer,
    isShuttingDown :: Shutdown,
    version :: DeploymentVersion,
    enableRedisLatencyLogging :: Bool,
    enablePrometheusMetricLogging :: Bool,
    cacheConfig :: CacheConfig,
    cacConfig :: CacConfig,
    kafkaProducerTools :: KafkaProducerTools,
    cacAclMap :: [(String, [(String, String)])],
    requestId :: Maybe Text,
    apiToken :: Text,
    slackEnv :: SlackEnv,
    shouldLogRequestId :: Bool,
    kafkaProducerForART :: Maybe KafkaProducerTools
  }
  deriving (Generic)

buildAppEnv :: AppCfg -> IO AppEnv
buildAppEnv AppCfg {..} = do
  podName <- getPodName
  version <- lookupDeploymentVersion
  loggerEnv <- prepareLoggerEnv loggerConfig podName
  esqDBEnv <- prepareEsqDBEnv esqDBCfg loggerEnv
  slackEnv <- createSlackConfig slackToken slackChannel
  esqDBReplicaEnv <- prepareEsqDBEnv esqDBReplicaCfg loggerEnv
  coreMetrics <- registerCoreMetricsContainer
  kafkaProducerTools <- buildKafkaProducerTools kafkaProducerCfg
  let modifierFunc = ("dashboard:" <>)
  let nonCriticalModifierFunc = ("dashboard:non-critical:" <>)
  let requestId = Nothing
  shouldLogRequestId <- fromMaybe False . (>>= readMaybe) <$> lookupEnv "SHOULD_LOG_REQUEST_ID"
  let kafkaProducerForART = Just kafkaProducerTools
  hedisEnv <- connectHedis hedisCfg modifierFunc
  hedisNonCriticalEnv <- connectHedis hedisNonCriticalCfg nonCriticalModifierFunc
  hedisClusterEnv <-
    if cutOffHedisCluster
      then pure hedisEnv
      else connectHedisCluster hedisClusterCfg modifierFunc
  hedisNonCriticalClusterEnv <-
    if cutOffHedisCluster
      then pure hedisNonCriticalEnv
      else connectHedisCluster hedisNonCriticalClusterCfg modifierFunc
  isShuttingDown <- mkShutdown
  cacAclMapRaw <- fromMaybe (error "AUTH_MAP not found in Env !!!!") <$> lookupEnv "AUTH_MAP"
  let cacAclMap = fromMaybe (error "Unable to Parse AUTH_MAP of CAC") (readMaybe cacAclMapRaw :: Maybe [(String, [(String, String)])])
  return $ AppEnv {..}

releaseAppEnv :: AppEnv -> IO ()
releaseAppEnv AppEnv {..} = do
  releaseLoggerEnv loggerEnv
  disconnectHedis hedisEnv
  disconnectHedis hedisClusterEnv

type FlowHandler = FlowHandlerR AppEnv

type FlowServer api = FlowServerR AppEnv api

type Flow = FlowR AppEnv
