-- | Per-city allocation tick job for shared cabs: a self-rescheduling scheduler job. The per-city lease
-- (Redis.whenWithLockRedis) guards the operator-reshard case where two pods would run the same tick.
module SharedLogic.Scheduler.Jobs.SharedCabAllocationTick
  ( sharedCabAllocationTick,
  )
where

import Data.IORef (newIORef, readIORef, writeIORef)
import Kernel.External.Types (SchedulerFlow, ServiceFlow)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Utils.Common
import Lib.Scheduler
import SharedLogic.JobScheduler
import SharedLogic.SharedCab.Allocation (InternalEndpointFlow, allocationPass)
import SharedLogic.SharedCab.Enabled (sharedCabEnabled, tickChainContinues)
import SharedLogic.SharedCab.FindingTimeout (CancelFlow, cancelTimedOutFindings)
import SharedLogic.SharedCab.LtsAttach (LtsFlow)
import SharedLogic.SharedCab.Schedule (claimTickRun, scheduleNextTick, thenReschedule, withCityTickLease)
import SharedLogic.SharedCab.StopProgress (runStopProgress)
import Storage.Beam.SchedulerJob ()

-- Seeded per city by Schedule.ensureAllocationTick on session open; keeps itself alive below.
sharedCabAllocationTick ::
  ( MonadFlow m,
    Redis.HedisFlow m r,
    Redis.HedisLTSFlowEnv r,
    SchedulerFlow r,
    ServiceFlow m r,
    HasFlowEnv m r '["maxNotificationShards" ::: Int],
    CacheFlow m r,
    EsqDBFlow m r,
    MonadMask m,
    HasKafkaProducer r,
    InternalEndpointFlow m r,
    Metrics.CoreMetrics m,
    HasField "blackListedJobs" r [Text],
    EncFlow m r,
    LtsFlow m r c,
    CancelFlow m r c
  ) =>
  Job 'SharedCabAllocationTick ->
  m ExecutionResult
sharedCabAllocationTick Job {jobInfo} = do
  let SharedCabAllocationTickJobData {merchantId, merchantOperatingCityId} = jobInfo.jobData
  -- a duplicate chain finds this tick claimed and ends. A suspended city allocates nothing new, but its chain keeps draining
  -- (timeouts, refunds, timers) until no live booking is left.
  claimed <- claimTickRun merchantOperatingCityId
  enabled <- sharedCabEnabled merchantOperatingCityId
  when claimed $ do
    -- stays True until a pass proves the chain can lapse, so a throwing tick still reschedules
    keepGoingRef <- liftIO $ newIORef True
    liveRef <- liftIO $ newIORef []
    -- the next tick is scheduled even when this one throws, or one DB hiccup ends the city's chain for good
    thenReschedule
      ( do
          -- the lease runSharedCabAllocationTick takes for on-demand triggers too; stop progress shares it
          withCityTickLease merchantOperatingCityId $
            allocationPass merchantOperatingCityId >>= \(live, positions) -> do
              liftIO (writeIORef liveRef live)
              liftIO (writeIORef keepGoingRef (tickChainContinues enabled (length live)))
              runStopProgress merchantOperatingCityId live positions
          -- after the lease, so the refund's network calls never hold up the city's tick
          liftIO (readIORef liveRef) >>= cancelTimedOutFindings merchantOperatingCityId
      )
      (liftIO (readIORef keepGoingRef) >>= \keepGoing -> when keepGoing (scheduleNextTick merchantId merchantOperatingCityId))
  pure Complete
