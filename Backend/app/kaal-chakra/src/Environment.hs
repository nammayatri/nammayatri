{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Environment
  ( HandlerCfg (..),
    HandlerEnv (..),
    SchedulerConfig (..),
    Flow,
    buildHandlerEnv,
    releaseHandlerEnv,
  )
where

import qualified Data.Map as M
import Data.String.Conversions (cs)
import Kernel.External.Encryption (EncTools)
import Kernel.Prelude
import Kernel.Storage.Clickhouse.Config
import Kernel.Storage.Esqueleto.Config
import Kernel.Storage.Hedis (HedisEnv, connectHedis, connectHedisCluster, disconnectHedis)
import Kernel.Streaming.Kafka.Producer.Types
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.Flow
import Kernel.Utils.App (lookupDeploymentVersion)
import Kernel.Utils.Common
import Kernel.Utils.Dhall (FromDhall)
import Kernel.Utils.IOLogging
import Lib.Scheduler (SchedulerType)
import Lib.Scheduler.Environment
import Lib.Yudhishthira.Types
import Passetto.Client
import System.Environment (lookupEnv)

data HandlerCfg = HandlerCfg
  { schedulerConfig :: SchedulerConfig,
    esqDBReplicaCfg :: EsqDBConfig,
    clickhouseCfg :: ClickhouseCfg,
    loggerConfigApp :: LoggerConfig,
    kvConfigUpdateFrequency :: Int,
    migrationPath :: [FilePath],
    autoMigrate :: Bool,
    jobInfoMapx :: M.Map Chakra Bool,
    cacheConfig :: CacheConfig,
    httpClientOptions :: HttpClientOptions,
    encTools :: EncTools,
    maxShards :: Int,
    shouldCreateJobs :: Bool,
    shouldCompleteOldJobs :: Bool
  }
  deriving (Generic, FromDhall)

data HandlerEnv = HandlerEnv
  { httpClientOptions :: HttpClientOptions,
    loggerEnv :: LoggerEnv,
    esqDBEnv :: EsqDBEnv,
    esqDBReplicaEnv :: EsqDBEnv,
    encTools :: EncTools,
    hedisEnv :: HedisEnv,
    hedisNonCriticalEnv :: HedisEnv,
    hedisNonCriticalClusterEnv :: HedisEnv,
    hedisClusterEnv :: HedisEnv,
    cutOffHedisCluster :: Bool,
    hedisMigrationStage :: Bool,
    cacheConfig :: CacheConfig,
    coreMetrics :: CoreMetricsContainer,
    maxShards :: Int,
    version :: DeploymentVersion,
    jobInfoMap :: M.Map Text Bool,
    enableRedisLatencyLogging :: Bool,
    enablePrometheusMetricLogging :: Bool,
    schedulerSetName :: Text,
    kvConfigUpdateFrequency :: Int,
    schedulerType :: SchedulerType,
    requestId :: Maybe Text,
    shouldLogRequestId :: Bool,
    serviceClickhouseEnv :: ClickhouseEnv,
    passettoContext :: PassettoContext,
    cacConfig :: CacConfig,
    kafkaProducerForART :: Maybe KafkaProducerTools,
    kafkaProducerTools :: KafkaProducerTools,
    groupName :: Text,
    streamName :: Text,
    shouldCreateJobs :: Bool,
    shouldCompleteOldJobs :: Bool
  }
  deriving (Generic)

buildHandlerEnv :: HandlerCfg -> IO HandlerEnv
buildHandlerEnv HandlerCfg {..} = do
  let SchedulerConfig {..} = schedulerConfig
  hostname <- fmap cs <$> lookupEnv "POD_NAME" :: IO (Maybe Text)
  version <- lookupDeploymentVersion
  loggerEnv <- prepareLoggerEnv loggerConfigApp hostname
  esqDBEnv <- prepareEsqDBEnv esqDBCfg loggerEnv
  esqDBReplicaEnv <- prepareEsqDBEnv esqDBReplicaCfg loggerEnv
  serviceClickhouseEnv <- createConn clickhouseCfg
  passettoContext <- (uncurry mkDefPassettoContext) encTools.service
  kafkaProducerTools <- buildKafkaProducerTools kafkaProducerCfg
  let kafkaProducerForART = Just kafkaProducerTools
  hedisEnv <- connectHedis hedisCfg ("kaal-chakra:" <>)
  hedisNonCriticalEnv <- connectHedis hedisNonCriticalCfg ("doa:n_c:" <>)
  let requestId = Nothing
  shouldLogRequestId <- fromMaybe False . (>>= readMaybe) <$> lookupEnv "SHOULD_LOG_REQUEST_ID"
  hedisClusterEnv <-
    if cutOffHedisCluster
      then pure hedisEnv
      else connectHedisCluster hedisClusterCfg ("kaal-chakra:" <>)
  hedisNonCriticalClusterEnv <-
    if cutOffHedisCluster
      then pure hedisNonCriticalEnv
      else connectHedisCluster hedisNonCriticalCfg ("doa:n_c:" <>)
  let jobInfoMap :: (M.Map Text Bool) = M.mapKeys show jobInfoMapx
  coreMetrics <- registerCoreMetricsContainer
  return HandlerEnv {..}

releaseHandlerEnv :: HandlerEnv -> IO ()
releaseHandlerEnv HandlerEnv {..} = do
  releaseLoggerEnv loggerEnv
  disconnectHedis hedisEnv
  disconnectHedis hedisClusterEnv

type Flow = FlowR HandlerEnv
