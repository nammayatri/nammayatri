-- M7.1 skeleton: Allocator-style per-city allocation TICK job for shared cabs.
--
-- Rider-app's own periodic-job shape (not the driver-app Allocator): a self-rescheduling
-- Scheduler job, modeled on SharedLogic.Scheduler.Jobs.FRFSSeatHoldReaper
-- (SharedLogic/Scheduler/Jobs/FRFSSeatHoldReaper.hs). Registration: riderJobType in
-- SharedLogic.JobScheduler + putJobHandlerInListWrapper in rider-app/Scheduler/src/App.hs.
--
-- Engine contract (05-allocation-plan §3): "one tick per city (Redis lease) + trigger on
-- booking create / release". The per-city lease is Redis.whenWithLockRedis: a pod that does
-- not hold it skips the tick silently (05 §8.5 one-tick-per-city; the DB-backed scheduler
-- already executes each job once per shard, the lease guards the operator-reshard case).
module SharedLogic.Scheduler.Jobs.SharedCabAllocationTick
  ( sharedCabAllocationTick,
  )
where

import Kernel.External.Types (SchedulerFlow, ServiceFlow)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (KafkaProducerTools)
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Utils.Common
import Lib.Scheduler
import Lib.Scheduler.JobStorageType.SchedulerType
import SharedLogic.JobScheduler
import SharedLogic.SharedCab.Allocation (runSharedCabAllocationTick)
import SharedLogic.SharedCab.Allocation.Types (defaultAllocationConfig)
import Storage.Beam.SchedulerJob ()

-- TODO(seed): nothing creates the first job per city yet (same bootstrap question existing
-- self-rescheduling jobs have). Once rider_config fields land (05 §7), seed on config change /
-- city enable; then the job keeps itself alive by re-scheduling below.
sharedCabAllocationTick ::
  ( MonadFlow m,
    Redis.HedisFlow m r,
    Redis.HedisLTSFlowEnv r,
    SchedulerFlow r,
    ServiceFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    MonadMask m,
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    Metrics.CoreMetrics m,
    HasField "blackListedJobs" r [Text]
  ) =>
  Job 'SharedCabAllocationTick ->
  m ExecutionResult
sharedCabAllocationTick Job {jobInfo} = do
  let jobData@SharedCabAllocationTickJobData {merchantId, merchantOperatingCityId} = jobInfo.jobData
      cfg = defaultAllocationConfig -- //TODO(05 §7): rider_config bind
      -- the city lease lives in runSharedCabAllocationTick, so on-demand triggers honour it too
  runSharedCabAllocationTick merchantOperatingCityId
  -- self-reschedule: 05 §7 tickSec (default 3 s)
  createJobIn @_ @'SharedCabAllocationTick (Just merchantId) (Just merchantOperatingCityId) (intToNominalDiffTime cfg.tickSec) jobData
  pure Complete
