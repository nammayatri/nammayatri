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
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Utils.Common
import Lib.Scheduler
import SharedLogic.JobScheduler
import SharedLogic.SharedCab.Allocation (InternalEndpointFlow, allocationPass, sharedCabAllocationEnabled, withCityTickLease)
import SharedLogic.SharedCab.AllocationSchedule (claimTickRun, scheduleNextTick)
import SharedLogic.SharedCab.LtsAttach (LtsFlow)
import SharedLogic.SharedCab.StopProgress (runStopProgress)
import Storage.Beam.SchedulerJob ()

-- Seeded per city by AllocationSchedule.ensureAllocationTick on session open; keeps itself alive below.
sharedCabAllocationTick ::
  ( MonadFlow m,
    Redis.HedisFlow m r,
    Redis.HedisLTSFlowEnv r,
    SchedulerFlow r,
    ServiceFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    MonadMask m,
    HasKafkaProducer r,
    InternalEndpointFlow m r,
    Metrics.CoreMetrics m,
    HasField "blackListedJobs" r [Text],
    EncFlow m r,
    LtsFlow m r c
  ) =>
  Job 'SharedCabAllocationTick ->
  m ExecutionResult
sharedCabAllocationTick Job {jobInfo} = do
  let SharedCabAllocationTickJobData {merchantId, merchantOperatingCityId} = jobInfo.jobData
  -- a duplicate chain finds this tick claimed and ends; the gate going off ends the chain too
  claimed <- claimTickRun merchantOperatingCityId
  when (claimed && sharedCabAllocationEnabled) $ do
    -- the lease runSharedCabAllocationTick takes for on-demand triggers too; stop progress (7.5) shares it
    withCityTickLease merchantOperatingCityId $
      allocationPass merchantOperatingCityId >>= uncurry (runStopProgress merchantOperatingCityId)
    scheduleNextTick merchantId merchantOperatingCityId
  pure Complete
