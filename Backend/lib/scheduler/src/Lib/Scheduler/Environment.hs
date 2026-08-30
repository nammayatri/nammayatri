{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.Scheduler.Environment where

import qualified Data.Map as M
import EulerHS.Interpreters (runFlow)
import qualified EulerHS.Language as L
import qualified EulerHS.Runtime as R
import Kernel.Beam.Connection.Flow (prepareConnectionDriver)
import Kernel.Beam.Connection.Types
import Kernel.Beam.Lib.UtilsTH
import Kernel.Beam.Types (KafkaConn (..))
import qualified Kernel.Beam.Types as KBT
import Kernel.Prelude
import Kernel.Storage.Beam.SystemConfigs
import Kernel.Storage.Esqueleto.Config
import Kernel.Storage.Hedis (HedisCfg, HedisEnv, HedisFlow, disconnectHedis)
import Kernel.Storage.Queries.SystemConfigs as QSC
import Kernel.Streaming.Kafka.Producer.Types
import Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Common
import Kernel.Types.Flow
import Kernel.Utils.Common
import qualified Kernel.Utils.Common as KUC
import Kernel.Utils.Dhall (FromDhall)
import qualified Kernel.Utils.FlowLogging as L
import Kernel.Utils.IOLogging (LoggerEnv, releaseLoggerEnv)
import Lib.Scheduler.JobStorageType.DB.Table
import Lib.Scheduler.Metrics
import Lib.Scheduler.Types

type JobInfoMap = M.Map Text Bool

data SchedulerConfig = SchedulerConfig
  { loggerConfig :: LoggerConfig,
    metricsPort :: Int,
    esqDBCfg :: EsqDBConfig,
    esqDBReplicaCfg :: EsqDBConfig,
    hedisCfg :: HedisCfg,
    hedisClusterCfg :: HedisCfg,
    hedisSecondaryClusterCfg :: HedisCfg,
    hedisNonCriticalCfg :: HedisCfg,
    hedisNonCriticalClusterCfg :: HedisCfg,
    hedisMigrationStage :: Bool,
    cutOffHedisCluster :: Bool,
    hedisPrefix :: Text,
    schedulerType :: SchedulerType,
    schedulerSetName :: Text,
    streamName :: Text,
    groupName :: Text,
    block :: Integer,
    readCount :: Integer,
    port :: Int,
    loopIntervalSec :: Seconds,
    maxThreads :: Int,
    expirationTime :: Integer,
    waitBeforeRetry :: Int,
    tasksPerIteration :: Int,
    graceTerminationPeriod :: Seconds,
    enableRedisLatencyLogging :: Bool,
    enablePrometheusMetricLogging :: Bool,
    cacConfig :: CacConfig,
    kafkaProducerCfg :: KafkaProducerCfg,
    secondaryKafkaProducerCfg :: Maybe KafkaProducerCfg,
    inMemConfig :: InMemConfig,
    blackListedJobs :: [Text],
    -- | How often (seconds) the per-pod reclaimer scans the PEL for idle entries to XCLAIM.
    reclaimIntervalSec :: Seconds,
    -- | Minimum idle time (ms) an entry must have been pending before it is eligible for reclaim.
    reclaimMinIdleMs :: Integer,
    -- | Max number of pending entries fetched per reclaim / sweep pass.
    reclaimBatch :: Integer,
    -- | How often (seconds) each pod heartbeats its consumer into the liveness registry.
    heartbeatIntervalSec :: Seconds,
    -- | A consumer whose last heartbeat is older than this (seconds) is considered dead and pruned.
    deadConsumerThresholdSec :: Integer,
    runConsumerCleanupThreads :: Bool
  }
  deriving (Generic, FromDhall)

data SchedulerEnv = SchedulerEnv
  { esqDBEnv :: EsqDBEnv,
    esqDBReplicaEnv :: EsqDBEnv,
    hedisEnv :: HedisEnv,
    hedisNonCriticalEnv :: HedisEnv,
    hedisNonCriticalClusterEnv :: HedisEnv,
    hedisClusterEnv :: HedisEnv,
    secondaryHedisClusterEnv :: Maybe HedisEnv,
    hedisMigrationStage :: Bool,
    cutOffHedisCluster :: Bool,
    coreMetrics :: Metrics.CoreMetricsContainer,
    loggerConfig :: LoggerConfig,
    loggerEnv :: LoggerEnv,
    schedulerType :: SchedulerType,
    schedulerSetName :: Text,
    kafkaProducerTools :: KafkaProducerTools,
    streamName :: Text,
    groupName :: Text,
    block :: Integer,
    readCount :: Integer,
    metrics :: SchedulerMetrics,
    loopIntervalSec :: Seconds,
    expirationTime :: Integer,
    waitBeforeRetry :: Int,
    tasksPerIteration :: Int,
    graceTerminationPeriod :: Seconds,
    consumerId :: Text,
    -- | Stable per-pod Redis stream consumer name (@version-podName@). Shared by all
    -- runner threads in this pod so the PEL is reclaimable across restarts.
    consumerName :: Text,
    reclaimIntervalSec :: Seconds,
    reclaimMinIdleMs :: Integer,
    reclaimBatch :: Integer,
    heartbeatIntervalSec :: Seconds,
    deadConsumerThresholdSec :: Integer,
    runConsumerCleanupThreads :: Bool,
    port :: Int,
    isShuttingDown :: Shutdown,
    version :: Metrics.DeploymentVersion,
    enableRedisLatencyLogging :: Bool,
    enablePrometheusMetricLogging :: Bool,
    maxThreads :: Int,
    maxShards :: Int,
    jobInfoMap :: JobInfoMap,
    kvConfigUpdateFrequency :: Int,
    cacheConfig :: CacheConfig,
    requestId :: Maybe Text,
    shouldLogRequestId :: Bool,
    sessionId :: Maybe Text,
    cacConfig :: CacConfig,
    kafkaProducerForART :: Maybe KafkaProducerTools,
    inMemEnv :: InMemEnv,
    url :: Maybe Text,
    blackListedJobs :: [Text],
    jobRetryOnExceptionMap :: JobInfoMap
  }
  deriving (Generic)

releaseSchedulerEnv :: SchedulerEnv -> IO ()
releaseSchedulerEnv SchedulerEnv {..} = do
  releaseLoggerEnv loggerEnv
  disconnectHedis hedisNonCriticalEnv
  disconnectHedis hedisNonCriticalClusterEnv
  disconnectHedis hedisEnv
  disconnectHedis hedisClusterEnv
  maybe (pure ()) disconnectHedis secondaryHedisClusterEnv

type SchedulerM = FlowR SchedulerEnv

type HasJobInfoMap r = HasField "jobInfoMap" r (M.Map Text Bool)

type JobCreatorEnv r = (HasJobInfoMap r, HasField "maxShards" r Int, HasField "schedulerSetName" r Text, HasField "blackListedJobs" r [Text])

type JobCreator r m = (JobCreatorEnv r, JobMonad r m)

type JobExecutorEnv r = (HasField "streamName" r Text, HasField "maxShards" r Int, HasField "groupName" r Text, HasField "schedulerSetName" r Text)

type JobExecutor r m = (JobExecutorEnv r, JobMonad r m)

-- 'CoreMetrics m' is required so that job creation/execution can emit the
-- scheduler_job_lifecycle_counter metric without every call site having to
-- thread the constraint through itself.
type JobMonad r m = (HasSchemaName SchedulerJobT, HasField "schedulerType" r SchedulerType, MonadReader r m, HedisFlow m r, MonadFlow m, EsqDBFlow m r, HasField "blackListedJobs" r [Text], Metrics.CoreMetrics m)

runSchedulerM :: HasSchemaName SystemConfigsT => SchedulerConfig -> SchedulerEnv -> SchedulerM a -> IO a
runSchedulerM schedulerConfig env action = do
  let loggerRt = L.getEulerLoggerRuntime Nothing env.loggerConfig
  R.withFlowRuntime (Just loggerRt) $ \flowRt -> do
    runFlow
      flowRt
      ( ( prepareConnectionDriver
            ConnectionConfigDriver
              { esqDBCfg = schedulerConfig.esqDBCfg,
                esqDBReplicaCfg = schedulerConfig.esqDBReplicaCfg,
                hedisClusterCfg = schedulerConfig.hedisClusterCfg,
                hedisSecondaryClusterCfg = schedulerConfig.hedisSecondaryClusterCfg
              }
            env.kvConfigUpdateFrequency
        )
          >> L.setOption KafkaConn env.kafkaProducerTools
      )
    flowRt' <- runFlowR flowRt env $ do
      fork
        "Fetching Kv configs"
        ( forever $ do
            handleExceptions $ do
              kvConfigs <- QSC.findById "kv_configs" >>= pure . decodeFromText' @Tables
              L.setOption KBT.Tables (fromMaybe KUC.defaultTableData kvConfigs)
            threadDelay (env.kvConfigUpdateFrequency * 1000000)
        )
      pure flowRt
    runFlowR flowRt' env action
  where
    handleExceptions = handle (\(e :: SomeException) -> L.logError ("KV_FETCH_FAILED_ALLOCATOR" :: Text) $ "Error fetching kv configs: " <> show e)
