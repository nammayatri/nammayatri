{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.Scheduler.App
  ( runSchedulerService,
  )
where

import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude hiding (mask, throwIO)
import Kernel.Randomizer
import Kernel.Storage.Beam.SystemConfigs
import Kernel.Storage.Esqueleto.Config (prepareEsqDBEnv)
import Kernel.Storage.Hedis (connectHedis, connectHedisCluster)
import Kernel.Streaming.Kafka.Producer.Types
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import qualified Kernel.Tools.Metrics.Init as Metrics
import Kernel.Types.CacheFlow
import Kernel.Types.Common (Seconds (..))
import qualified Kernel.Types.MonadGuid as G
import Kernel.Utils.App
import Kernel.Utils.Common (threadDelaySec)
import Kernel.Utils.IOLogging (prepareLoggerEnv)
import Kernel.Utils.Servant.Server
import Kernel.Utils.Shutdown
import Lib.Scheduler.Environment
import Lib.Scheduler.Handler (SchedulerHandle, handler)
import Lib.Scheduler.Metrics
import Lib.Scheduler.Types (JobProcessor)
import Servant (Context (EmptyContext))
import System.Exit
import UnliftIO

runSchedulerService ::
  (JobProcessor t, FromJSON t, HasSchemaName SystemConfigsT) =>
  SchedulerConfig ->
  JobInfoMap ->
  Int ->
  Int ->
  SchedulerHandle t ->
  IO ()
runSchedulerService s@SchedulerConfig {..} jobInfoMap kvConfigUpdateFrequency maxShards handle_ = do
  hostname <- getPodName
  version <- lookupDeploymentVersion
  loggerEnv <- prepareLoggerEnv loggerConfig hostname
  esqDBEnv <- prepareEsqDBEnv esqDBCfg loggerEnv
  coreMetrics <- Metrics.registerCoreMetricsContainer
  kafkaProducerTools <- buildKafkaProducerTools kafkaProducerCfg
  hedisEnv <- connectHedis hedisCfg (\k -> hedisPrefix <> ":" <> k)
  hedisNonCriticalEnv <- connectHedis hedisNonCriticalCfg (\k -> hedisPrefix <> ":" <> k)
  hedisNonCriticalClusterEnv <-
    if cutOffHedisCluster
      then pure hedisNonCriticalEnv
      else connectHedisCluster hedisNonCriticalClusterCfg (\k -> hedisPrefix <> ":" <> k)
  hedisClusterEnv <-
    if cutOffHedisCluster
      then pure hedisEnv
      else connectHedisCluster hedisClusterCfg (\k -> hedisPrefix <> ":" <> k)
  metrics <- setupSchedulerMetrics
  isShuttingDown <- mkShutdown
  consumerId <- G.generateGUIDTextIO
  let cacheConfig = CacheConfig {configsExpTime = 0}
  let schedulerEnv = SchedulerEnv {cacheConfig, ..}
  when (tasksPerIteration <= 0) $ do
    hPutStrLn stderr ("tasksPerIteration should be greater than 0" :: Text)
    exitFailure
  Metrics.serve metricsPort
  let serverStartAction = handler handle_
  randSecDelayBeforeStart <- Seconds <$> getRandomInRange (0, loopIntervalSec.getSeconds)
  threadDelaySec randSecDelayBeforeStart -- to make runners start out_of_sync to reduce probability of picking same tasks.
  withAsync (runSchedulerM s schedulerEnv serverStartAction) $ \schedulerAction ->
    runServerGeneric
      schedulerEnv
      (Proxy @HealthCheckAPI)
      healthCheck
      identity
      identity
      EmptyContext
      (const identity)
      (\_ -> cancel schedulerAction)
      (runSchedulerM s)

-- TODO: explore what is going on here
-- To which thread do signals come?
