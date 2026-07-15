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

import qualified Control.Monad.Catch as C
import qualified Database.Redis as Redis
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude hiding (mask, throwIO)
import Kernel.Randomizer
import Kernel.Storage.Beam.SystemConfigs
import Kernel.Storage.Esqueleto.Config (prepareEsqDBEnv)
import Kernel.Storage.Hedis (connectHedis, connectHedisCluster)
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Storage.InMem as IM
import Kernel.Streaming.Kafka.Producer.Types
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import qualified Kernel.Tools.Metrics.Init as Metrics
import Kernel.Types.CacheFlow
import Kernel.Types.Common (Seconds (..))
import qualified Kernel.Types.MonadGuid as G
import Kernel.Utils.App
import Kernel.Utils.Common (logError, threadDelaySec)
import Kernel.Utils.IOLogging (prepareLoggerEnv)
import Kernel.Utils.Servant.Server
import Kernel.Utils.Shutdown
import Lib.Scheduler.Environment
import Lib.Scheduler.Handler (SchedulerHandle, handler)
import Lib.Scheduler.Metrics
import Lib.Scheduler.Types (JobProcessor)
import Servant (Context (EmptyContext), ServerError (..), err503)
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
  esqDBReplicaEnv <- prepareEsqDBEnv esqDBReplicaCfg loggerEnv
  coreMetrics <- Metrics.registerCoreMetricsContainer
  kafkaProducerTools <- buildKafkaProducerTools kafkaProducerCfg secondaryKafkaProducerCfg
  let kafkaProducerForART = Just kafkaProducerTools
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
  secondaryHedisClusterEnv <-
    Kernel.Prelude.try (connectHedisCluster hedisSecondaryClusterCfg (\k -> hedisPrefix <> ":" <> k)) >>= \case
      Left (e :: Kernel.Prelude.SomeException) -> do
        putStrLn $ "ERROR: Failed to connect to secondary hedis cluster: " ++ show e
        pure Nothing
      Right env -> pure (Just env)
  metrics <- setupSchedulerMetrics
  isShuttingDown <- mkShutdown
  consumerId <- G.generateGUIDTextIO
  let requestId = Nothing
      sessionId = Nothing
      shouldLogRequestId = False
  let cacheConfig = CacheConfig {configsExpTime = 0}
  inMemEnv <- IM.setupInMemEnv inMemConfig (Just hedisClusterEnv)
  let url = Nothing
  let schedulerEnv = SchedulerEnv {cacheConfig, esqDBReplicaEnv, ..}
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
      (schedulerHealthCheck schedulerAction)
      identity
      identity
      EmptyContext
      (const identity)
      (\_ -> cancel schedulerAction)
      (runSchedulerM s)

-- | Health check for the scheduler/allocator service.
--
-- Verifies two liveness properties and only then returns HTTP 200:
--   1. The scheduler looper thread is still running (i.e. it has not
--      crashed or exited). @poll@ returns 'Nothing' while the async is
--      still running and @Just@ once it has finished (normally or with an
--      exception).
--   2. Redis is reachable (a @PING@ round-trips to @PONG@).
--
-- On any failure it throws @err503@ so the endpoint reports the service as
-- unhealthy (5xx) instead of a misleading 200.
schedulerHealthCheck :: Async () -> SchedulerM Text
schedulerHealthCheck schedulerAction = do
  -- (1) Looper thread liveness.
  mLooperResult <- poll schedulerAction
  whenJust mLooperResult $ \looperResult -> do
    logError $ "SCHEDULER_HEALTHCHECK_FAILED: looper thread is not running, result: " <> show looperResult
    C.throwM err503 {errBody = "Scheduler looper thread is not running"}
  -- (2) Redis connectivity.
  eRedis <- Kernel.Prelude.try $ Hedis.withCrossAppRedis $ Hedis.runHedis Redis.ping
  case eRedis of
    Right Redis.Pong -> pure "App is up"
    Right otherStatus -> do
      logError $ "SCHEDULER_HEALTHCHECK_FAILED: unexpected redis ping response: " <> show otherStatus
      C.throwM err503 {errBody = "Scheduler redis connectivity check failed"}
    Left (e :: SomeException) -> do
      logError $ "SCHEDULER_HEALTHCHECK_FAILED: redis ping failed: " <> show e
      C.throwM err503 {errBody = "Scheduler redis connectivity check failed"}

-- TODO: explore what is going on here
-- To which thread do signals come?
