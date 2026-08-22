{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Canonical per-ride cancellation signals, computed once and shared by every consumer
-- that reasons about a cancellation: the driver coin policy (CANCELLATION_COIN_POLICY)
-- and the customer dues policy (USER_CANCELLATION_DUES). The same physical event must
-- look identical to every rule set, so any change to a signal definition here changes
-- the inputs of all of them at once.
module SharedLogic.CancellationSignals where

import qualified Data.Text as Text
import qualified Domain.Types.Ride as DRide
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Yudhishthira.Types as LYT
import qualified SharedLogic.BehaviourManagement.PickupStallState as PickupStallState
import qualified Storage.Queries.CallStatus as QCallStatus
import qualified Storage.Queries.DriverQuote as QDQ

data CancellationSignals = CancellationSignals
  { computedAt :: UTCTime,
    timeOfCancellation :: Int,
    timeSinceBooking :: Maybe Int,
    isArrivedAtPickup :: Bool,
    driverWaitingTime :: Maybe Int,
    callAttemptByDriver :: Bool,
    callAttemptCount :: Int,
    actualCoveredDistance :: Maybe Meters,
    expectedCoveredDistance :: Maybe Meters,
    -- raw distances (also exposed to the fault rules: e.g. "driver within
    -- max(10% of initial, 100m) of pickup" style conditions)
    initialDistanceToPickup :: Maybe Meters,
    currentDistanceToPickup :: Maybe Meters,
    isAdvanceBooking :: Bool,
    isPickupOrDestinationEdited :: Bool,
    pickupStallCase :: Maybe Text
  }
  deriving (Generic, Show)

data CancellationSignalsReq = CancellationSignalsReq
  { ride :: DRide.Ride,
    quoteId :: Text,
    bookingCreatedAt :: Maybe UTCTime,
    fallbackDurationToPickup :: Maybe Seconds,
    initialDisToPickup :: Maybe Meters,
    cancellationDisToPickup :: Maybe Meters,
    arrivedPickupThreshold :: HighPrecMeters
  }

buildCancellationSignals :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => CancellationSignalsReq -> m CancellationSignals
buildCancellationSignals req = do
  now <- getCurrentTime
  callAttemptCount <- getCallAttemptCount req.ride.id
  durationToPickup <- maybe (fromMaybe 0 req.fallbackDurationToPickup) (.durationToPickup) <$> QDQ.findById (Id req.quoteId)
  pickupStallCase <- getPickupStallCase req.ride
  let computedAt = now
      estimatedTimeToPickup = secondsToNominalDiffTime durationToPickup
      timeOfCancellation = round $ diffUTCTime now req.ride.createdAt
      callAttemptByDriver = callAttemptCount > 0
      actualCoveredDistance = case (req.initialDisToPickup, req.cancellationDisToPickup) of
        (Just initial, Just cancellation) -> Just (initial - cancellation)
        _ -> Nothing
      expectedCoveredDistance =
        req.initialDisToPickup <&> \initialDistance ->
          let progressRatio = fromIntegral timeOfCancellation / max 1 estimatedTimeToPickup
           in round $ fromIntegral initialDistance * progressRatio
      driverWaitingTime = req.ride.driverArrivalTime <&> \arrivalTime -> round $ diffUTCTime now arrivalTime
      isDistanceArrived = maybe False (< highPrecMetersToMeters req.arrivedPickupThreshold) req.cancellationDisToPickup
      isArrivedAtPickup = isJust req.ride.driverArrivalTime || isDistanceArrived
      timeSinceBooking = req.bookingCreatedAt <&> \createdAt -> round $ diffUTCTime now createdAt
      initialDistanceToPickup = req.initialDisToPickup
      currentDistanceToPickup = req.cancellationDisToPickup
      isAdvanceBooking = req.ride.isAdvanceBooking
      isPickupOrDestinationEdited = fromMaybe False req.ride.isPickupOrDestinationEdited
  pure CancellationSignals {..}

-- | The one definition of "the driver attempted to call the rider" — every cancellation
-- consumer (tags, coins, dues, penalty preview) must derive from this count.
getCallAttemptCount :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DRide.Ride -> m Int
getCallAttemptCount rideId = QCallStatus.countCallsByEntityId rideId

getCallAttemptByDriver :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DRide.Ride -> m Bool
getCallAttemptByDriver rideId = (> 0) <$> getCallAttemptCount rideId

-- | The stall verdict for this ride, if any: the case name (STALLED / RETREATING /
-- LOCATION_DARK) from the PickupStallDetected ride tag, falling back to the pickup
-- progress monitor's live Redis state for stalls detected but not yet fired as a tag.
getPickupStallCase :: (MonadFlow m, CacheFlow m r) => DRide.Ride -> m (Maybe Text)
getPickupStallCase ride = do
  let tagPrefix = PickupStallState.pickupStallRideTagPrefix <> "#"
      tagCase = listToMaybe $ mapMaybe (\(LYT.TagNameValue t) -> Text.stripPrefix tagPrefix t) (fromMaybe [] ride.rideTags)
  case tagCase of
    Just stallCase -> pure (Just stallCase)
    Nothing -> do
      mbState :: Maybe PickupStallState.PickupProgressState <- Redis.safeGet (PickupStallState.pickupProgressStateKey ride.id)
      pure $ (.activeCase) =<< mbState
