{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Internal.BulkLocUpdate where

import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromJust)
import Data.OpenApi (ToSchema)
import Domain.Action.UI.Ride.EndRide.Internal (getRouteInfoWithShortestDuration)
import qualified Domain.Types as DC
import Domain.Types.BatchPriority (BatchPriority (..))
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import Environment
import EulerHS.Prelude
import Kernel.External.Maps.Types
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.LocationUpdates
import qualified Lib.LocationUpdates as LocUpd
import qualified SharedLogic.CallBAP as CallBAP
import SharedLogic.Ride as SRide
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.Ride as QRide
import qualified Tools.Maps as TM
import Tools.Utils (isDropInsideThreshold)

data BulkLocUpdateReq = BulkLocUpdateReq
  { rideId :: Id DRide.Ride,
    driverId :: Id DP.Person,
    loc :: NonEmpty LatLong,
    -- Trace propagation fields
    batchTraceId :: Maybe Text,
    clientBatchedAt :: Maybe UTCTime,
    ltsReceivedAt :: Maybe UTCTime
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

-- | Telemetry event emitted to Kafka batch-telemetry topic
data BatchTelemetryEvent = BatchTelemetryEvent
  { batchTraceId :: Text,
    stage :: Text,
    backendReceivedAt :: UTCTime,
    backendProcessedAt :: UTCTime,
    snapToRoadCalled :: Bool,
    pointsInBatch :: Int,
    merchantId :: Text,
    cityId :: Text
  }
  deriving (Generic, ToJSON, FromJSON, Show)

-- | Circuit breaker state for snap-to-road calls
data CircuitBreakerState = CircuitBreakerState
  { consecutiveFailures :: Int,
    state :: CBState,
    lastFailureAt :: Maybe UTCTime
  }
  deriving (Generic, ToJSON, FromJSON, Show)

data CBState = CBClosed | CBOpen | CBHalfOpen
  deriving (Generic, ToJSON, FromJSON, Show, Eq)

-- | Determine priority based on active ride status
determinePriority :: DRide.Ride -> DC.TripCategory -> BatchPriority
determinePriority ride tripCategory =
  if ride.status == DRide.INPROGRESS
    then case tripCategory of
      DC.OneWay DC.MeterRide -> Critical
      _ -> High
    else case ride.status of
      DRide.NEW -> Medium
      _ -> Low

-- | Coalesce multiple updates for same driver - keep only the latest location per driver
coalesceDriverUpdates :: [BulkLocUpdateReq] -> [BulkLocUpdateReq]
coalesceDriverUpdates reqs =
  let grouped = HM.toList $ foldl' groupByDriver HM.empty reqs
   in map snd grouped
  where
    groupByDriver acc req =
      HM.insertWith keepLatest req.driverId.getId req acc
    keepLatest newReq oldReq =
      -- Merge locations: old locations followed by new, keeping all waypoints
      oldReq {loc = oldReq.loc <> newReq.loc}

-- | Get circuit breaker state from Redis
getCircuitBreakerState :: (MonadFlow m, CacheFlow m r) => Text -> m CircuitBreakerState
getCircuitBreakerState key = do
  mbState :: Maybe CircuitBreakerState <- Redis.safeGet key
  pure $ fromMaybe defaultCBState mbState
  where
    defaultCBState = CircuitBreakerState {consecutiveFailures = 0, state = CBClosed, lastFailureAt = Nothing}

-- | Update circuit breaker state in Redis
updateCircuitBreakerState :: (MonadFlow m, CacheFlow m r) => Text -> CircuitBreakerState -> m ()
updateCircuitBreakerState key cbState = Redis.setExp key cbState 120

-- | Check if snap-to-road should be attempted given circuit breaker state
shouldAttemptSnapToRoad :: (MonadFlow m, CacheFlow m r) => Text -> m (Bool, CircuitBreakerState)
shouldAttemptSnapToRoad merchantKey = do
  cbState <- getCircuitBreakerState merchantKey
  now <- getCurrentTime
  case cbState.state of
    CBClosed -> pure (True, cbState)
    CBOpen -> do
      let cooldownElapsed = case cbState.lastFailureAt of
            Just lastFail -> diffUTCTime now lastFail > 30
            Nothing -> True
      if cooldownElapsed
        then do
          let halfOpenState = cbState {state = CBHalfOpen}
          updateCircuitBreakerState merchantKey halfOpenState
          pure (True, halfOpenState)
        else pure (False, cbState)
    CBHalfOpen -> pure (True, cbState)

-- | Record snap-to-road success for circuit breaker
recordSnapToRoadSuccess :: (MonadFlow m, CacheFlow m r) => Text -> m ()
recordSnapToRoadSuccess merchantKey = do
  let closedState = CircuitBreakerState {consecutiveFailures = 0, state = CBClosed, lastFailureAt = Nothing}
  updateCircuitBreakerState merchantKey closedState

-- | Record snap-to-road failure for circuit breaker
recordSnapToRoadFailure :: (MonadFlow m, CacheFlow m r) => Text -> m ()
recordSnapToRoadFailure merchantKey = do
  cbState <- getCircuitBreakerState merchantKey
  now <- getCurrentTime
  let newFailures = cbState.consecutiveFailures + 1
  let newState =
        if newFailures >= 3
          then CBOpen
          else cbState.state
  let updatedState = CircuitBreakerState {consecutiveFailures = newFailures, state = newState, lastFailureAt = Just now}
  updateCircuitBreakerState merchantKey updatedState

mkCircuitBreakerKey :: Text -> Text -> Text
mkCircuitBreakerKey merchantId cityId =
  "SnapToRoadCB:merchantId:" <> merchantId <> ":cityId:" <> cityId

bulkLocUpdate :: BulkLocUpdateReq -> Flow APISuccess
bulkLocUpdate req = do
  backendReceivedAt <- getCurrentTime
  let driverId = req.driverId
      rideId = req.rideId
      loc = req.loc
  logDebug $ "BulkLocUpdate = " <> show rideId <> " " <> show driverId <> " " <> show loc
  ride <- QRide.findById rideId >>= fromMaybeM (RideNotFound rideId.getId)
  transportConfig <- SCTC.findByMerchantOpCityId ride.merchantOperatingCityId Nothing >>= fromMaybeM (TransporterConfigNotFound ride.merchantOperatingCityId.getId)
  booking <- QBooking.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  merchantId <- fromMaybeM (InternalError "Ride does not have a merchantId") $ ride.merchantId

  -- Priority-based processing
  let priority = determinePriority ride booking.tripCategory
  logDebug $ "BulkLocUpdate priority = " <> show priority <> " for ride " <> show rideId

  let minUpdatesToTriggerSnapToRoad = getMinLocUpdateCountForDistanceCalculation transportConfig ride.tripCategory
  defaultRideInterpolationHandler <- LocUpd.buildRideInterpolationHandler merchantId ride.merchantOperatingCityId (Just rideId) False (Just minUpdatesToTriggerSnapToRoad)
  rectificationServiceConfig <-
    if DC.shouldRectifyDistantPointsSnapToRoadFailure booking.tripCategory
      then Just <$> TM.getServiceConfigForRectifyingSnapToRoadDistantPointsFailure booking.providerId booking.merchantOperatingCityId
      else pure Nothing
  let isTollApplicable = DC.isTollApplicableForTrip booking.vehicleServiceTier booking.tripCategory
  let passedThroughDrop = any (isDropInsideThreshold booking transportConfig) loc
  logDebug $ "Did we passed through drop yet in bulkLocation  " <> show passedThroughDrop <> " and points: " <> show loc

  -- Snap-to-road circuit breaker check
  let cbKey = mkCircuitBreakerKey merchantId.getId ride.merchantOperatingCityId.getId
  (shouldSnap, _cbState) <- shouldAttemptSnapToRoad cbKey
  snapToRoadCalled <- if shouldSnap
    then do
      catch
        ( do
            _ <- addIntermediateRoutePoints defaultRideInterpolationHandler rectificationServiceConfig isTollApplicable transportConfig.enableTollCrossedNotifications rideId driverId passedThroughDrop (booking.tripCategory == DC.OneWay DC.MeterRide) loc
            recordSnapToRoadSuccess cbKey
            pure True
        )
        ( \(err :: SomeException) -> do
            logError $ "Snap-to-road failed, using raw coordinates: " <> show err
            recordSnapToRoadFailure cbKey
            pure False
        )
    else do
      logDebug $ "Snap-to-road circuit breaker open, using raw coordinates for ride " <> show rideId
      -- Still process route points without snap-to-road
      _ <- addIntermediateRoutePoints defaultRideInterpolationHandler rectificationServiceConfig isTollApplicable transportConfig.enableTollCrossedNotifications rideId driverId passedThroughDrop (booking.tripCategory == DC.OneWay DC.MeterRide) loc
      pure False

  -- Emit telemetry event when trace id is present
  whenJust req.batchTraceId $ \traceId -> do
    backendProcessedAt <- getCurrentTime
    let telemetryEvent =
          BatchTelemetryEvent
            { batchTraceId = traceId,
              stage = "haskell_backend",
              backendReceivedAt = backendReceivedAt,
              backendProcessedAt = backendProcessedAt,
              snapToRoadCalled = snapToRoadCalled,
              pointsInBatch = length loc,
              merchantId = merchantId.getId,
              cityId = ride.merchantOperatingCityId.getId
            }
    logInfo $ "BatchTelemetry: " <> show telemetryEvent

  let buffertime' = getArrivalTimeBufferOfVehicle transportConfig.arrivalTimeBufferOfVehicle booking.vehicleServiceTier
  when (isJust buffertime' && ride.status == DRide.INPROGRESS && isJust ride.estimatedEndTimeRange && isJust ride.toLocation) $ do
    now <- getCurrentTime
    let buffertime = fromJust buffertime'
    let endTimeRange = fromJust ride.estimatedEndTimeRange
    when (now > addUTCTime (secondsToNominalDiffTime (div buffertime 2)) endTimeRange.start && not passedThroughDrop) $
      fork "update estimated end time" $ do
        logDebug $ "Updating estimated end time for ride " <> show rideId
        let toLocation = fromJust ride.toLocation
            currentLatLong = NE.last loc
            dropLatLong = TM.LatLong {lat = toLocation.lat, lon = toLocation.lon}
        routeResponse <-
          TM.getRoutes merchantId booking.merchantOperatingCityId (Just rideId.getId) $
            TM.GetRoutesReq
              { waypoints = NE.fromList [currentLatLong, dropLatLong],
                mode = Just TM.CAR,
                calcPoints = True
              }
        shortestRoute <- getRouteInfoWithShortestDuration routeResponse & fromMaybeM (InternalError "No route found for latlongs")
        (duration :: Seconds) <- shortestRoute.duration & fromMaybeM (InternalError "No duration found for new route")
        let newEstimatedEndTimeRange = SRide.calculateEstimatedEndTimeRange now duration transportConfig.arrivalTimeBufferOfVehicle booking.vehicleServiceTier
        let updatedRide = ride {DRide.estimatedEndTimeRange = newEstimatedEndTimeRange}
        QRide.updateEstimatedEndTimeRange newEstimatedEndTimeRange rideId
        CallBAP.sendRideEstimatedEndTimeRangeUpdateToBAP booking updatedRide
  pure Success
  where
    getMinLocUpdateCountForDistanceCalculation transporterConfig tripCategory =
      case tripCategory of
        (DC.OneWay DC.MeterRide) -> transporterConfig.meterRideBulkLocUpdateBatchSize
        _ -> transporterConfig.normalRideBulkLocUpdateBatchSize
