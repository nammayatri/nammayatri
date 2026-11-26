{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | A shared-route run whose driver drops out goes back to the scheduled board, not to CANCELLED.
--
-- Every way a run loses its driver ends in 'SharedLogic.Cancel.reAllocateBookingIfPossible' after the cancel
-- transaction (driver cancel, ops unassign, the activation job's cancel, the pickup-stall monitor, the fallback job's
-- unassign of an unacknowledged run); its SharedRoute arm calls 'returnRunToBoard'. The run keeps its booking: the
-- status goes back to NEW, the board entry is rewritten, the cancel marker is cleared as a reused booking's is, and the
-- fallback job is asked to cover it now. No BAP is told and no search is broadcast: a run has no rider-side ride, and
-- its passengers stay attached through the trip and departure (see "Domain.Types.SharedRoute".'RunRef').
module SharedLogic.SharedRun.Reassign
  ( ReassignFlow,
    returnRunToBoard,
    droppedDrivers,
  )
where

import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.BookingCancellationReason as DBCR
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import SharedLogic.Allocator (AllocatorJobType (..), SchedulerJobFlow, SharedRunFallbackJobData (..))
import qualified SharedLogic.Booking as SBooking
import qualified SharedLogic.ScheduledBooking.OpsNote as OpsNote
import qualified SharedLogic.SearchTryLocker as CS
import qualified SharedLogic.SharedRun.Acknowledgement as Ack
import qualified Storage.Queries.Booking as QRB

type ReassignFlow m r =
  ( MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    SchedulerJobFlow r,
    OpsNote.OpsNoteFlow m r
  )

-- | True: the run is back on the board and the fallback job will cover it, so the caller must not report a cancelled
-- booking. False: the run was already due, nothing changed here, and the caller cancels it as before.
returnRunToBoard :: ReassignFlow m r => DRB.Booking -> DRide.Ride -> DBCR.BookingCancellationReason -> m Bool
returnRunToBoard booking ride bookingCReason = do
  now <- getCurrentTime
  if now >= booking.startTime
    then do
      logInfo $ "shared run " <> booking.id.getId <> " lost its driver at/after departure; cancelled"
      OpsNote.addSystemOpsNote booking $ "Run cancelled at/after departure: the driver dropped it (" <> show bookingCReason.source <> ")"
      pure False
    else do
      QRB.updateStatus booking.id DRB.NEW
      let onBoard = booking {DRB.status = DRB.NEW, DRB.updatedAt = now}
      SBooking.removeBookingFromRedis booking
      SBooking.addScheduledBookingInRedis onBoard
      -- the booking is reused, so the cancel's marker would block the next driver's claim
      CS.markBookingCancellationCompleted booking.id
      recordDroppedDriver booking ride.driverId
      Ack.clearAcknowledgement ride.id
      createJobIn @_ @'SharedRunFallback (Just booking.providerId) (Just booking.merchantOperatingCityId) 1 $
        SharedRunFallbackJobData {bookingId = booking.id, reason = "driver dropped the run (" <> show bookingCReason.source <> ")"}
      logInfo $ "shared run " <> booking.id.getId <> " dropped by driver " <> ride.driverId.getId <> " (" <> show bookingCReason.source <> "); back on the board"
      pure True

droppedKey :: Id DRB.Booking -> Text
droppedKey bookingId = "sharedRun:dropped:" <> bookingId.getId

-- | Kept until a day after departure; the fallback job does not offer the run to these drivers again.
recordDroppedDriver :: (CacheFlow m r, MonadFlow m) => DRB.Booking -> Id DP.Person -> m ()
recordDroppedDriver booking driverId = do
  now <- getCurrentTime
  let ttl = max 60 (ceiling (diffUTCTime (addUTCTime 86400 booking.startTime) now))
  Hedis.withMasterRedis $ do
    void $ Hedis.sAdd (droppedKey booking.id) [driverId.getId]
    Hedis.expire (droppedKey booking.id) ttl

-- | The drivers who dropped this run.
droppedDrivers :: (CacheFlow m r, MonadFlow m) => Id DRB.Booking -> m [Id DP.Person]
droppedDrivers bookingId = map Id <$> Hedis.withMasterRedis (Hedis.sMembers @Text (droppedKey bookingId))
