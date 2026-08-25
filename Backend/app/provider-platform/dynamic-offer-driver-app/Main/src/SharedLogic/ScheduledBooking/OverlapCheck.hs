{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Multi-hold overlap checks; committed set derived from ride+booking, candidate needs only a travel+buffer gap vs its two neighbours.
module SharedLogic.ScheduledBooking.OverlapCheck where

import Data.List (sortOn)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import Domain.Types.TransporterConfig (TransporterConfig)
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.Ride as QRide
import qualified Tools.Maps as TMaps

-- | One committed ride as a time interval anchored at its pickup/drop positions.
data CommittedInterval = CommittedInterval
  { intervalStart :: UTCTime,
    intervalEnd :: UTCTime,
    intervalPickup :: LatLong,
    intervalDrop :: LatLong
  }
  deriving (Generic, Show)

-- | The ride being considered (a new scheduled search or a board booking).
data ScheduledCandidate = ScheduledCandidate
  { candidateStart :: UTCTime,
    candidateEnd :: UTCTime,
    candidatePickup :: LatLong,
    candidateDrop :: Maybe LatLong
  }
  deriving (Generic, Show)

committedRideStatuses :: [DRide.RideStatus]
committedRideStatuses = [DRide.UPCOMING, DRide.NEW, DRide.INPROGRESS]

-- | The driver's committed set, sorted by scheduled pickup time. Derived on demand — always fresh.
getDriverCommittedRides :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DP.Person -> m [(DRide.Ride, DRB.Booking)]
getDriverCommittedRides driverId = do
  rides <- QRide.findAllByDriverIdAndStatuses driverId committedRideStatuses
  bookings <- QBooking.findAllByIds (map (.bookingId) rides)
  let pairs = mapMaybe (\ride -> (\booking -> (ride, booking)) <$> find (\b -> b.id == ride.bookingId) bookings) rides
  pure $ sortOn ((.startTime) . snd) pairs

-- | Committed sets for many drivers, keyed by driver, each sorted by pickup time.
type CommittedRidesByDriver = Map.Map (Id DP.Person) [(DRide.Ride, DRB.Booking)]

-- | Batched form of 'getDriverCommittedRides' for a whole pool: one ride read + one booking read, grouped by driver.
getDriverCommittedRidesForDrivers :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => [Id DP.Person] -> m CommittedRidesByDriver
getDriverCommittedRidesForDrivers [] = pure Map.empty
getDriverCommittedRidesForDrivers driverIds = do
  rides <- QRide.findAllByDriverIdsAndStatuses driverIds committedRideStatuses
  bookings <- QBooking.findAllByIds (map (.bookingId) rides)
  let pairs = mapMaybe (\ride -> (\booking -> (ride.driverId, (ride, booking))) <$> find (\b -> b.id == ride.bookingId) bookings) rides
  pure $ Map.map (sortOn ((.startTime) . snd)) (Map.fromListWith (++) [(drvId, [pair]) | (drvId, pair) <- pairs])

-- | Holds = UPCOMING rides only (NEW/INPROGRESS are the trip underway, not a held slot).
countActiveHolds :: [(DRide.Ride, DRB.Booking)] -> Int
countActiveHolds = length . filter (\(ride, _) -> ride.status == DRide.UPCOMING)

mkCommittedIntervals :: UTCTime -> Maybe (Id DRB.Booking) -> [(DRide.Ride, DRB.Booking)] -> [CommittedInterval]
mkCommittedIntervals now mbExcludeBookingId = sortOn (.intervalStart) . mapMaybe mk
  where
    mk (ride, booking)
      | maybe False (booking.id ==) mbExcludeBookingId = Nothing
      | otherwise =
        let duration = maybe 0 secondsToNominalDiffTime booking.estimatedDuration
            pickupPos = LatLong booking.fromLocation.lat booking.fromLocation.lon
            dropPos = maybe pickupPos (\loc -> LatLong loc.lat loc.lon) booking.toLocation
         in case ride.status of
              DRide.UPCOMING -> Just $ CommittedInterval booking.startTime (addUTCTime duration booking.startTime) pickupPos dropPos
              DRide.NEW -> Just $ CommittedInterval booking.startTime (addUTCTime duration booking.startTime) pickupPos dropPos
              -- trip underway occupies [now, planned end]; it can only ever be a predecessor, so its drop is what matters
              DRide.INPROGRESS ->
                let plannedEnd = addUTCTime duration (fromMaybe booking.startTime ride.tripStartTime)
                 in Just $ CommittedInterval now (max now plannedEnd) pickupPos dropPos
              _ -> Nothing

mkCandidateFromBooking :: DRB.Booking -> ScheduledCandidate
mkCandidateFromBooking booking =
  ScheduledCandidate
    { candidateStart = booking.startTime,
      candidateEnd = addUTCTime (maybe 0 secondsToNominalDiffTime booking.estimatedDuration) booking.startTime,
      candidatePickup = LatLong booking.fromLocation.lat booking.fromLocation.lon,
      candidateDrop = (\loc -> LatLong loc.lat loc.lon) <$> booking.toLocation
    }

-- | Candidate must clear (end + deadhead + buffer) vs its predecessor and successor; unknown leg fails closed.
isCandidateFeasible ::
  (MonadFlow m, ServiceFlow m r) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  TransporterConfig ->
  ScheduledCandidate ->
  [CommittedInterval] ->
  m Bool
isCandidateFeasible merchantId merchantOpCityId transporterConfig candidate committed = do
  let (mbPredecessor, mbSuccessor) = selectNeighbours candidate.candidateStart committed
  predecessorOk <- case mbPredecessor of
    Nothing -> pure True
    Just predecessor -> legFeasible predecessor.intervalDrop predecessor.intervalEnd candidate.candidatePickup candidate.candidateStart
  if not predecessorOk
    then pure False
    else case mbSuccessor of
      Nothing -> pure True
      Just successor -> case candidate.candidateDrop of
        Nothing -> pure False -- unknown drop: cannot prove the gap to the next hold
        Just candidateDrop -> legFeasible candidateDrop candidate.candidateEnd successor.intervalPickup successor.intervalStart
  where
    buffer = transporterConfig.scheduleRideBufferTime
    legFeasible fromPos fromEnd toPos toStart
      | legInfeasibleByTime buffer fromEnd toStart = pure False -- even a zero-length deadhead cannot make it
      | otherwise = do
        resp <-
          TMaps.getDistanceForScheduledRides merchantId merchantOpCityId Nothing $
            TMaps.GetDistanceReq
              { origin = fromPos,
                destination = toPos,
                travelMode = Just TMaps.CAR,
                sourceDestinationMapping = Nothing,
                distanceUnit = Meter
              }
        pure $ legFeasibleWithDistance transporterConfig resp.distance fromEnd toStart

-- | Predecessor = latest committed interval starting at/before the candidate; successor = earliest strictly after.
selectNeighbours :: UTCTime -> [CommittedInterval] -> (Maybe CommittedInterval, Maybe CommittedInterval)
selectNeighbours candidateStart committed =
  ( listToMaybe . reverse $ filter (\ci -> ci.intervalStart <= candidateStart) committed,
    listToMaybe $ filter (\ci -> ci.intervalStart > candidateStart) committed
  )

-- | The gap can't fit even a zero-length deadhead — buffer alone already overruns the next start.
legInfeasibleByTime :: NominalDiffTime -> UTCTime -> UTCTime -> Bool
legInfeasibleByTime buffer fromEnd toStart = addUTCTime buffer fromEnd > toStart

-- | Given a precomputed deadhead distance, does travel + buffer still clear the next start?
legFeasibleWithDistance :: TransporterConfig -> Meters -> UTCTime -> UTCTime -> Bool
legFeasibleWithDistance transporterConfig distance fromEnd toStart =
  let avgSpeedKmph = fromMaybe 25.0 transporterConfig.scheduledRideConfig.avgSpeedKmph
      buffer = transporterConfig.scheduleRideBufferTime
      deadheadKm = (fromIntegral distance.getMeters :: Double) / 1000
      deadheadSeconds = realToFrac (deadheadKm / avgSpeedKmph * 3600) :: NominalDiffTime
   in addUTCTime (deadheadSeconds + buffer) fromEnd <= toStart

-- | Per-leg pre-classification before any OSRM call: no neighbour on this side, rejected on time alone, or needs a distance cell (carrying the point that varies across the pool + the two times for the final check).
data LegPrecheck
  = LegClearedNoNeighbour
  | LegRejectedByTime
  | LegNeedsDistance !LatLong !UTCTime !UTCTime

-- | A driver either has a pure verdict (no OSRM needed) or survives to the distance phase carrying its optional predecessor/successor cell needs.
data DriverClass a
  = ClassDecided a !Bool
  | ClassNeedsDistance a !(Maybe (LatLong, UTCTime, UTCTime)) !(Maybe (LatLong, UTCTime, UTCTime))

-- | Pool-level feasibility for one shared candidate against many drivers' committed sets, collapsing the per-driver point-to-point OSRM calls into at most two batched @/table@ calls (predecessor legs many-to-one on the pickup, successor legs one-to-many from the drop). Input order is preserved; a missing/errored distance cell fails closed, matching the single-driver path's verdict.
batchIsCandidateFeasible ::
  (MonadFlow m, ServiceFlow m r) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  TransporterConfig ->
  ScheduledCandidate ->
  [(a, [CommittedInterval])] ->
  m [(a, Bool)]
batchIsCandidateFeasible merchantId merchantOpCityId transporterConfig candidate inputs = do
  let classified = map (\(payload, committed) -> classifyDriver buffer candidate payload committed) inputs
      predPoints = [point | ClassNeedsDistance _ (Just (point, _, _)) _ <- classified]
      succPoints = [point | ClassNeedsDistance _ _ (Just (point, _, _)) <- classified]
  -- predecessor batch: origins = predecessor drops, single destination = candidate pickup; key each cell by the echoed resp.origin so we never depend on the provider preserving request order (Google route-matrix may omit/reorder)
  predDistByPoint <- case NE.nonEmpty predPoints of
    Nothing -> pure Map.empty
    Just originsPts -> do
      resps <- callTable originsPts (candidate.candidatePickup :| [])
      pure $ Map.fromList [(pointKey resp.origin, resp.distance) | resp <- resps]
  -- successor batch: single origin = candidate drop, destinations = successor pickups; key each cell by the echoed resp.destination
  succDistByPoint <- case (candidate.candidateDrop, NE.nonEmpty succPoints) of
    (Just cDrop, Just destsPts) -> do
      resps <- callTable (cDrop :| []) destsPts
      pure $ Map.fromList [(pointKey resp.destination, resp.distance) | resp <- resps]
    _ -> pure Map.empty
  pure $ map (evaluate predDistByPoint succDistByPoint) classified
  where
    buffer = transporterConfig.scheduleRideBufferTime
    pointKey p = (p.lat, p.lon)
    callTable origins destinations = do
      res <-
        withTryCatch "batchScheduledRideDistances" $
          TMaps.getDistancesForScheduledRides merchantId merchantOpCityId Nothing $
            TMaps.GetDistancesReq
              { origins = origins,
                destinations = destinations,
                travelMode = Just TMaps.CAR,
                sourceDestinationMapping = Nothing,
                distanceUnit = Meter
              }
      pure $ either (const []) NE.toList res
    evaluate _ _ (ClassDecided payload verdict) = (payload, verdict)
    evaluate predDistByPoint succDistByPoint (ClassNeedsDistance payload predNeed succNeed) =
      (payload, legOk predDistByPoint predNeed && legOk succDistByPoint succNeed)
    legOk _ Nothing = True
    legOk distByPoint (Just (point, fromEnd, toStart)) = case Map.lookup (pointKey point) distByPoint of
      Nothing -> False -- errored, dropped (non-ROUTE_EXISTS), or missing cell: fail closed, exactly as the single-driver unknown-leg case
      Just distance -> legFeasibleWithDistance transporterConfig distance fromEnd toStart

-- | Classify one driver's committed set against the shared candidate without any OSRM call.
classifyDriver :: NominalDiffTime -> ScheduledCandidate -> a -> [CommittedInterval] -> DriverClass a
classifyDriver buffer candidate payload committed =
  let (mbPredecessor, mbSuccessor) = selectNeighbours candidate.candidateStart committed
      predLeg = case mbPredecessor of
        Nothing -> LegClearedNoNeighbour
        Just predecessor -> classifyLeg buffer predecessor.intervalDrop predecessor.intervalEnd candidate.candidateStart
      succLeg = case mbSuccessor of
        Nothing -> LegClearedNoNeighbour
        Just successor -> case candidate.candidateDrop of
          Nothing -> LegRejectedByTime -- unknown drop: cannot prove the gap to the next hold
          Just _ -> classifyLeg buffer successor.intervalPickup candidate.candidateEnd successor.intervalStart
   in case (predLeg, succLeg) of
        (LegRejectedByTime, _) -> ClassDecided payload False
        (_, LegRejectedByTime) -> ClassDecided payload False
        (predResolved, succResolved) -> ClassNeedsDistance payload (toNeed predResolved) (toNeed succResolved)
  where
    toNeed (LegNeedsDistance point fromEnd toStart) = Just (point, fromEnd, toStart)
    toNeed _ = Nothing

-- | Pre-check one leg on time alone; if it survives, record the point that varies across the pool (batched later) plus the times for the final distance check.
classifyLeg :: NominalDiffTime -> LatLong -> UTCTime -> UTCTime -> LegPrecheck
classifyLeg buffer varyingPoint fromEnd toStart
  | legInfeasibleByTime buffer fromEnd toStart = LegRejectedByTime
  | otherwise = LegNeedsDistance varyingPoint fromEnd toStart

-- | Earliest remaining UPCOMING hold, excluding the one being released.
earliestRemainingScheduledHold :: Maybe (Id DRB.Booking) -> [(DRide.Ride, DRB.Booking)] -> Maybe (UTCTime, LatLong)
earliestRemainingScheduledHold mbExcludeBookingId pairs =
  listToMaybe
    [ (booking.startTime, LatLong booking.fromLocation.lat booking.fromLocation.lon)
      | (ride, booking) <- pairs,
        ride.status == DRide.UPCOMING,
        maybe True (booking.id /=) mbExcludeBookingId
    ]

-- | New latest_scheduled_booking after releasing a hold; single-slot short-circuits to Nothing (no fetch).
nextScheduledHoldAfterRelease :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => TransporterConfig -> Id DP.Person -> Id DRB.Booking -> m (Maybe (UTCTime, LatLong))
nextScheduledHoldAfterRelease transporterConfig driverId releasedBookingId
  | transporterConfig.scheduledRideConfig.maxHoldsPerDriver <= 1 = pure Nothing
  | otherwise = earliestRemainingScheduledHold (Just releasedBookingId) <$> getDriverCommittedRides driverId
