module SharedLogic.Scheduler.Jobs.FRFSSeatHoldReaper
  ( frfsSeatHoldReaper,
    seatHoldReaperImpl,
  )
where

import Kernel.External.Types (SchedulerFlow, ServiceFlow)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Utils.Common
import Lib.Scheduler
import Lib.Scheduler.JobStorageType.SchedulerType
import SharedLogic.FRFSSeatBooking
  ( ActiveSeatHold (..),
    releaseHold,
    timerKey,
  )
import SharedLogic.JobScheduler
import Storage.Beam.SchedulerJob ()

frfsSeatHoldReaper ::
  ( MonadFlow m,
    Hedis.HedisFlow m r,
    SchedulerFlow r,
    ServiceFlow m r,
    HasField "blackListedJobs" r [Text]
  ) =>
  Job 'FRFSSeatHoldReaper ->
  m ExecutionResult
frfsSeatHoldReaper Job {jobInfo} = do
  let jobData = jobInfo.jobData
      merchantId' = jobData.merchantId
      merchantOperatingCityId' = jobData.merchantOperatingCityId
  void $ seatHoldReaperImpl
  createJobIn @_ @'FRFSSeatHoldReaper (Just merchantId') (Just merchantOperatingCityId') (intToNominalDiffTime 120) (jobData :: FRFSSeatHoldReaperJobData)
  logDebug "Scheduled next FRFSSeatHoldReaper job to run in 120 seconds"
  pure Complete

seatHoldReaperImpl ::
  ( MonadFlow m,
    Hedis.HedisFlow m r
  ) =>
  m ()
seatHoldReaperImpl = do
  holds <- Hedis.sMembers "active-seat-holds"
  forM_ holds $ \(ActiveSeatHold tripId holdId) ->
    processHold tripId holdId

processHold ::
  (MonadFlow m, Hedis.HedisFlow m r) =>
  Text ->
  Text ->
  m ()
processHold tripId holdId = do
  let tKey = timerKey tripId holdId
  timerTtl <- Hedis.ttl tKey
  -- TTL == -2 → timer key does not exist (expired)
  when (timerTtl <= -1) $ do
    logInfo $ "SeatHoldReaper:processHold expired holdId=" <> holdId <> " tripId=" <> tripId
    releaseHold tripId holdId
