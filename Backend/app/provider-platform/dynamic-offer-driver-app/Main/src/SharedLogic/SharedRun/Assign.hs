{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Handing a shared-route run to a driver: 'assignRun' is the scheduled accept's own steps, 'assignFirstAvailable' tries
-- a list of candidates in order until one takes it. The materialiser gives it the drivers of the vehicles bound to the
-- run's route; the fallback job gives it the owned fleet minus anyone who dropped the run.
module SharedLogic.SharedRun.Assign
  ( AssignFlow,
    RunCandidate (..),
    Assignment (..),
    assignFirstAvailable,
    assignRun,
    missingRunIdentity,
  )
where

import qualified Data.Text as T
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.TransporterConfig as DTC
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import SharedLogic.Allocator (AllocatorJobType (..), ScheduledRideAssignedOnUpdateJobData (..))
import qualified SharedLogic.Booking as SBooking
import qualified SharedLogic.Ride as SRide
import qualified SharedLogic.ScheduledBooking.OverlapCheck as SBOC
import qualified SharedLogic.SearchTryLocker as CS
import Storage.Beam.SchedulerJob ()
import Tools.Error

type AssignFlow m r c = SRide.InitializeRideFlow m r c

-- | One driver the run may go to, with the fleet owner the ride is assigned under and a label for logs and ops notes.
data RunCandidate = RunCandidate
  { driver :: DP.Person,
    fleetOwnerId :: Id DP.Person,
    label :: Text
  }

data Assignment
  = -- | the candidate's label
    Assigned Text
  | -- | why nobody took the run
    Unassigned Text
  deriving (Show, Eq)

-- | The first candidate that takes the run, in order. A candidate that cannot (an overlap, a lost claim, a missing
-- vehicle) is skipped with its reason; the run stays on the scheduled board when nobody takes it.
assignFirstAvailable :: AssignFlow m r c => DM.Merchant -> DTC.TransporterConfig -> DRB.Booking -> [RunCandidate] -> m Assignment
assignFirstAvailable merchant transporterConfig booking = tryEach []
  where
    tryEach errors [] = pure (Unassigned (T.intercalate "; " (reverse errors)))
    tryEach errors (candidate : rest) = do
      res <- withTryCatch "sharedRun:assignFirstAvailable" $ assignRun merchant transporterConfig booking candidate.driver candidate.fleetOwnerId
      case res of
        Right _ -> do
          logInfo $ "shared run " <> booking.id.getId <> " handed to " <> candidate.label
          pure (Assigned candidate.label)
        Left err -> tryEach ((candidate.label <> ": " <> show err) : errors) rest

-- | The scheduled accept as a fleet owner's dashboard assignment runs it, step for step with the scheduled branch of
-- Domain.Action.UI.Driver.acceptStaticOfferDriverRequest (the two must move together): the overlap guard, off the
-- scheduled board, the assignment claim, ride creation and the driver's gate under the driver's hold lock, then the
-- pre-ride job. The checks on a driver choosing a ride for themselves (wallet floor, reachability, prepaid credits) do
-- not apply: the vehicle is bound to the route.
assignRun :: AssignFlow m r c => DM.Merchant -> DTC.TransporterConfig -> DRB.Booking -> DP.Person -> Id DP.Person -> m ()
assignRun merchant transporterConfig booking driver fleetOwnerId = do
  SBOC.ensureNoScheduledOverlap merchant.id driver.id transporterConfig booking
  SBooking.removeBookingFromRedis booking
  claimed <- CS.tryMarkBookingAssignmentInprogress booking.id
  unless claimed $ throwError RideRequestAlreadyAccepted
  (ride, _, _) <-
    CS.withDriverScheduledHoldLock driver.id $ do
      -- authoritative re-check under the lock; on a lost race, put the run back on the board before rejecting
      recheck <- withTryCatch "sharedRun:assignOverlapLocked" $ SBOC.ensureNoScheduledOverlap merchant.id driver.id transporterConfig booking
      case recheck of
        Right _ -> pure ()
        Left exc -> do
          CS.markBookingAssignmentCompleted booking.id
          SBooking.addScheduledBookingInRedis booking
          throwM exc
      res <- SRide.initializeRide merchant driver booking Nothing Nothing Nothing Nothing (Just fleetOwnerId) False False (Just DRide.AssignedByFleetOwner)
      SBOC.updateLatestScheduledAsMin driver.id booking
      pure res
  -- the rider platform finds the run's passengers by the trip and feed the ride carries, not by the ride id; a ride
  -- without them would show its driver an empty run
  let missing = missingRunIdentity ride
  unless (null missing) $
    logError $ "shared run " <> booking.id.getId <> ": ride " <> ride.id.getId <> " was created without " <> T.intercalate ", " missing <> "; its passengers cannot be found"
  now <- getCurrentTime
  let jobScheduledTime = max 2 (diffUTCTime booking.startTime now - transporterConfig.scheduleRideBufferTime)
  createJobIn @_ @'ScheduledRideAssignedOnUpdate (Just merchant.id) (Just booking.merchantOperatingCityId) jobScheduledTime $
    ScheduledRideAssignedOnUpdateJobData {driverId = driver.id, bookingId = booking.id, rideId = ride.id}
  CS.markBookingAssignmentCompleted booking.id

-- | The run identity a ride must copy from its run booking (see "Domain.Types.SharedRoute".'RunRef'), by name, when absent.
missingRunIdentity :: DRide.Ride -> [Text]
missingRunIdentity ride =
  [name | (name, present) <- [("tripId", isJust ride.tripId), ("routeCode", isJust ride.routeCode), ("integratedBppConfigId", isJust ride.integratedBppConfigId)], not present]
