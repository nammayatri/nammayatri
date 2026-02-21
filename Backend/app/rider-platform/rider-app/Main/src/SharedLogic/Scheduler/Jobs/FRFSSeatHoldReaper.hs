module SharedLogic.Scheduler.Jobs.FRFSSeatHoldReaper
  ( frfsSeatHoldReaper,
  )
where

import Data.Aeson as Ae
import qualified Data.ByteString as BSL
import qualified Database.Redis as RawRedis
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Utils.Common (MonadFlow)
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
  reapFromSet RawRedis.cursor0
  return Complete

reapFromSet ::
  (MonadFlow m, Hedis.HedisFlow m r) =>
  RawRedis.Cursor ->
  m ()
reapFromSet cursor = do
  (newCursor, encodedIds) <- Hedis.sscanOpts "active-seat-holds" cursor 100
  forM_ encodedIds $ \encodedId ->
    case Ae.decode (BSL.fromStrict encodedId) of
      Nothing ->
        pure ()
      Just (ActiveSeatHold tripId holdId) ->
        processHold tripId holdId
  unless (newCursor == RawRedis.cursor0) $ do
    reapFromSet newCursor

processHold ::
  (MonadFlow m, Hedis.HedisFlow m r) =>
  Text ->
  Text ->
  m ()
processHold tripId holdId = do
  let tKey = timerKey tripId holdId
  timerTtl <- Hedis.ttl tKey
  -- TTL == -2 → timer key does not exist (expired)
  when (timerTtl == -2) $
    releaseHold tripId holdId
