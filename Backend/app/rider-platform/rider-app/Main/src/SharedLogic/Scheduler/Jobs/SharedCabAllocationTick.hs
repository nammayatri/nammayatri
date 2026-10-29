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
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Utils.Common
import Lib.Scheduler
import SharedLogic.JobScheduler
import SharedLogic.SharedCab.Allocation (InternalEndpointFlow, allocationPass)
import SharedLogic.SharedCab.Enabled (sharedCabEnabled, tickChainContinues)
import SharedLogic.SharedCab.FindingTimeout (CancelFlow, cancelTimedOutFindings)
import SharedLogic.SharedCab.LtsAttach (LtsFlow)
import SharedLogic.SharedCab.Schedule (claimTickRun, claimedTick, ensureExpiryJob, ownAllocationChain, scheduleNextTick, withCityTickLease)
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
  let SharedCabAllocationTickJobData {merchantId, merchantOperatingCityId, chainId = mbChainId, leaseMisses} = jobInfo.jobData
  -- a chain the guard no longer names ends here. A suspended city allocates nothing new, but its chain keeps draining
  -- (timeouts, refunds, timers) until no live booking is left.
  ownAllocationChain merchantId merchantOperatingCityId mbChainId $ \chainId -> do
    -- stays True until a pass proves the chain can lapse, so a throwing tick still reschedules
    keepGoingRef <- liftIO $ newIORef True
    liveRef <- liftIO $ newIORef []
    -- stays True unless the lease was held, so a throwing tick reschedules as usual
    gotLeaseRef <- liftIO $ newIORef True
    scheduledRef <- liftIO $ newIORef False
    let scheduleNext = do
          keepGoing <- liftIO (readIORef keepGoingRef)
          gotLease <- liftIO (readIORef gotLeaseRef)
          liftIO (writeIORef scheduledRef True)
          when keepGoing (scheduleNextTick merchantId merchantOperatingCityId chainId gotLease leaseMisses)
    claimedTick
      (claimTickRun merchantOperatingCityId leaseMisses)
      ( do
          enabled <- sharedCabEnabled merchantOperatingCityId
          -- the lease runSharedCabAllocationTick takes for on-demand triggers too; stop progress shares it
          gotLease <-
            withCityTickLease merchantOperatingCityId $
              allocationPass merchantOperatingCityId >>= \(live, positions) -> do
                liftIO (writeIORef liveRef live)
                liftIO (writeIORef keepGoingRef (tickChainContinues enabled (length live)))
                runStopProgress merchantOperatingCityId live positions
          liftIO (writeIORef gotLeaseRef gotLease)
          -- before the refunds' payment calls, so a slow gateway never delays the city's next tick
          scheduleNext
          -- after the lease, so the refund's network calls never hold up the city's tick
          liftIO (readIORef liveRef) >>= cancelTimedOutFindings merchantOperatingCityId
          -- the expiry chain's watchdog: a lost expiry job would otherwise wait for the stream reclaim
          withTryCatch "sharedCab:allocationWatchesExpiry" (ensureExpiryJob merchantId merchantOperatingCityId)
            >>= either (logError . ("sharedCab tick: expiry watchdog failed: " <>) . show) pure
      )
      (liftIO (readIORef scheduledRef) >>= \scheduled -> unless scheduled scheduleNext)
  pure Complete
