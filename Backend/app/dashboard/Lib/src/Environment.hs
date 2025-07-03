{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Environment where

import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import Domain.Types.ServerName
import Kernel.External.Encryption (EncTools)
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config
import Kernel.Storage.Hedis (HedisCfg, HedisEnv, connectHedis, connectHedisCluster, disconnectHedis)
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
import Passetto.Client
import System.Environment
import Tools.Metrics
import Tools.Streaming.Kafka

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
    internalAuthAPIKey :: Text,
    registrationTokenExpiry :: Days,
    registrationTokenInactivityTimeout :: Maybe Seconds,
    encTools :: EncTools,
    exotelToken :: Text,
    dataServers :: [DataServer],
    merchantUserAccountNumber :: Int,
    enableRedisLatencyLogging :: Bool,
    enablePrometheusMetricLogging :: Bool,
    slackToken :: Text,
    slackChannel :: Text,
    internalEndPointMap :: M.Map BaseUrl BaseUrl,
    cacheConfig :: CacheConfig,
    cacConfig :: CacConfig,
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
    internalAuthAPIKey :: Text,
    registrationTokenExpiry :: Days,
    registrationTokenInactivityTimeout :: Maybe Seconds,
    encTools :: EncTools,
    coreMetrics :: Metrics.CoreMetricsContainer,
    isShuttingDown :: Shutdown,
    authTokenCacheKeyPrefix :: Text,
    exotelToken :: Text,
    dataServers :: [DataServer],
    merchantUserAccountNumber :: Int,
    version :: DeploymentVersion,
    enableRedisLatencyLogging :: Bool,
    enablePrometheusMetricLogging :: Bool,
    slackEnv :: SlackEnv,
    internalEndPointHashMap :: HM.HashMap BaseUrl BaseUrl,
    cacheConfig :: CacheConfig,
    cacConfig :: CacConfig,
    kafkaProducerTools :: KafkaProducerTools,
    cacAclMap :: [(String, [(String, String)])],
    requestId :: Maybe Text,
    shouldLogRequestId :: Bool,
    kafkaProducerForART :: Maybe KafkaProducerTools,
    passettoContext :: PassettoContext
  }
  deriving (Generic)

buildAppEnv :: Text -> AppCfg -> IO AppEnv
buildAppEnv authTokenCacheKeyPrefix AppCfg {..} = do
  podName <- getPodName
  version <- lookupDeploymentVersion
  loggerEnv <- prepareLoggerEnv loggerConfig podName
  passettoContext <- (uncurry mkDefPassettoContext) encTools.service
  esqDBEnv <- prepareEsqDBEnv esqDBCfg loggerEnv
  esqDBReplicaEnv <- prepareEsqDBEnv esqDBReplicaCfg loggerEnv
  coreMetrics <- registerCoreMetricsContainer
  slackEnv <- createSlackConfig slackToken slackChannel
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
  let internalEndPointHashMap = HM.fromList $ M.toList internalEndPointMap
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
