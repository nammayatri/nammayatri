module SharedLogic.Scheduler.Jobs.FRFSSeatHoldReaper
  ( frfsSeatHoldReaper,
  )
where

import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Utils.Common (MonadFlow, logInfo)
import Lib.Scheduler
import SharedLogic.FRFSSeatBooking
  ( ActiveSeatHold (..),
    releaseHold,
    timerKey,
  )
import SharedLogic.JobScheduler (RiderJobType (..))

frfsSeatHoldReaper ::
  ( MonadFlow m,
    Hedis.HedisFlow m r
  ) =>
  Job 'FRFSSeatHoldReaper ->
  m ExecutionResult
frfsSeatHoldReaper _job = do
  holds <- Hedis.sMembers "active-seat-holds"
  forM_ holds $ \(ActiveSeatHold tripId holdId) ->
    processHold tripId holdId
  pure Complete

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
