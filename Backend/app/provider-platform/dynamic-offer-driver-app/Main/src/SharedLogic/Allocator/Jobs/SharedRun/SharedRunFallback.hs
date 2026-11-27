{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Keeps one shared-route run covered. Runs at the run's fallback time (SharedRun.Knobs.fallbackDue, scheduled by
-- SharedLogic.SharedRun.Coverage.scheduleCoverageJobs) and at once whenever the run loses its driver
-- (SharedLogic.SharedRun.Reassign.returnRunToBoard). What it does is 'decideFallback':
--
-- * a driver holds the run but did not acknowledge by the fallback time: unassign them through the ordinary cancel
--   (the activation job's own path); the cancel puts the run back on the board and runs this job again;
-- * nobody holds it: offer it to the owned fleet ('Coverage.ownedFleetCandidates', through the scheduled accept's
--   steps); if nobody takes it, it stays on the scheduled board for curated drivers. At the fallback time it is offered
--   once to the city's ordinary drivers as a ride request ('Broadcast.offerRunToDrivers'); if it is still uncovered at
--   the last look before the scheduled-ride buffer closes, ops are told once.
--
-- Idempotent: every step re-reads the run, and one run's jobs never overlap (a per-run lock).
module SharedLogic.Allocator.Jobs.SharedRun.SharedRunFallback
  ( SharedRunFallbackFlow,
    sharedRunFallback,
  )
where

import qualified Data.Text as T
import qualified Domain.Action.UI.Ride.CancelRide as RideCancel
import qualified Domain.Types.Booking as Fields
import qualified Domain.Types.Ride as Fields
import qualified Domain.Types.TransporterConfig as Fields
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Scheduler
import SharedLogic.Allocator
import SharedLogic.Allocator.Jobs.ScheduledRides.ScheduledRideAssignedOnUpdate (ScheduledRideAssignedOnUpdateFlow, cancelOrReallocate)
import qualified SharedLogic.Booking as SBooking
import qualified SharedLogic.SharedRun.Assign as Assign
import qualified SharedLogic.SharedRun.Broadcast as Broadcast
import qualified SharedLogic.SharedRun.Coverage as Coverage
import SharedLogic.SharedRun.Coverage.Rules (FallbackDecision (..), NoAssignmentStep (..), afterNoAssignment, decideFallback)
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.Booking as QRB

type SharedRunFallbackFlow m r c =
  ( ScheduledRideAssignedOnUpdateFlow m r c,
    Assign.AssignFlow m r c,
    Coverage.CoverageFlow m r
  )

sharedRunFallback :: SharedRunFallbackFlow m r c => Job 'SharedRunFallback -> m ExecutionResult
sharedRunFallback Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) $ do
  let jobData = jobInfo.jobData
      lockKey = "sharedRun:fallback:lock:" <> jobData.bookingId.getId
  locked <- Hedis.tryLockRedis lockKey 60
  if not locked
    then do
      -- another run of this job holds the run; look again shortly rather than racing it
      now <- getCurrentTime
      pure (ReSchedule (addUTCTime 10 now))
    else finally (cover jobData) (Hedis.unlockRedis lockKey)
  where
    cover jobData =
      QRB.findById jobData.bookingId >>= \case
        Nothing -> pure (Terminate $ "shared run " <> jobData.bookingId.getId <> " not found")
        Just booking ->
          Coverage.runSnapshot booking >>= \case
            Nothing -> pure (Terminate "the run's city has no shared-run supply knobs")
            Just snapshot -> act jobData snapshot
    act jobData snapshot = do
      let booking = snapshot.booking
          tag = "shared run " <> booking.id.getId <> " (" <> jobData.reason <> "): "
      case (decideFallback snapshot.state, snapshot.ride) of
        (StopFallback why, _) -> do
          logInfo $ tag <> "nothing to cover: " <> why
          pure Complete
        (KeepAssignment, _) -> do
          logInfo $ tag <> "covered"
          pure Complete
        (UnassignUnacknowledged, Just ride) -> do
          logInfo $ tag <> "driver " <> ride.driverId.getId <> " did not acknowledge; unassigning"
          cancelOrReallocate ride "Shared run not acknowledged before the fallback window" True (RideCancel.ApplicationRequestorId id.getId)
          pure Complete
        (UnassignUnacknowledged, Nothing) -> pure Complete
        (FindDriver, _) -> do
          merchant <- CQM.findById booking.providerId >>= fromMaybeM (MerchantNotFound booking.providerId.getId)
          candidates <- Coverage.ownedFleetCandidates booking
          outcome <-
            if null candidates
              then pure (Assign.Unassigned "no owned-fleet driver is free")
              else Assign.assignFirstAvailable merchant snapshot.transporterConfig booking candidates
          case outcome of
            Assign.Assigned who -> do
              logInfo $ tag <> "covered by " <> who
              pure Complete
            Assign.Unassigned why -> do
              -- stays on the scheduled board, where curated drivers see it
              SBooking.addScheduledBookingInRedis booking
              let step = afterNoAssignment snapshot.state (Broadcast.alreadyOffered booking) snapshot.transporterConfig.scheduleRideBufferTime
              logInfo $ tag <> "not covered by the owned fleet: " <> why
              -- then the city's ordinary drivers, as a ride request; a failed offer goes straight to ops
              offerFailed <-
                if step.broadcast
                  then
                    withTryCatch "sharedRun:offerRunToDrivers" (Broadcast.offerRunToDrivers merchant booking) >>= \case
                      Right () -> pure False
                      Left err -> do
                        logError $ tag <> "could not offer the run to the driver pool: " <> show err
                        pure True
                  else pure False
              when (step.alertOps || offerFailed) $
                Coverage.alertOpsOnce booking $ "Run not covered: " <> T.take 200 why
              pure $ maybe Complete ReSchedule step.recheckAt
