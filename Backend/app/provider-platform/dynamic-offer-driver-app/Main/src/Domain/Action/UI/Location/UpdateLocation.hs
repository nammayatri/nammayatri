{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Domain.Action.UI.Location.UpdateLocation
  ( UpdateLocationReq,
    RideStatus,
    Waypoint (..),
    UpdateLocationHandle (..),
    buildUpdateLocationHandle,
    updateLocationHandler,
  )
where

import qualified Data.List.NonEmpty as NE
import qualified Domain.Types.Driver.DriverFlowStatus as DDFS
import Domain.Types.DriverFee
import qualified Domain.Types.DriverInformation as DDInfo
import Domain.Types.DriverLocation (DriverLocation)
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Ride as DRide
import Environment (Flow)
import GHC.Records.Extra
import Kernel.Beam.Functions
import qualified Kernel.Beam.Functions as B
import Kernel.External.Maps.Types
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer (produceMessage)
import Kernel.Streaming.Kafka.Producer.Types (KafkaProducerTools)
import Kernel.Types.APISuccess (APISuccess (..))
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Types.SlidingWindowLimiter (APIRateLimitOptions)
import Kernel.Utils.CalculateDistance
import Kernel.Utils.Common hiding (id)
import Kernel.Utils.GenericPretty (PrettyShow)
import Kernel.Utils.SlidingWindowLimiter (slidingWindowLimiter)
import qualified Lib.LocationUpdates as LocUpd
import SharedLogic.DriverFee (mergeDriverFee)
import qualified SharedLogic.DriverLocation as DrLoc
import SharedLogic.DriverPool (updateDriverSpeedInRedis)
import qualified SharedLogic.Ride as SRide
import Storage.CachedQueries.DriverInformation (updatePendingPayment, updateSubscription)
import qualified Storage.CachedQueries.DriverInformation as DInfo
import qualified Storage.CachedQueries.Merchant.TransporterConfig as QTConf
import qualified Storage.Queries.Driver.DriverFlowStatus as QDFS
import Storage.Queries.DriverFee (findOldestFeeByStatus, findOngoingAfterEndTime, findUnpaidAfterPayBy, updateStatus)
import qualified Storage.Queries.Person as QP

type UpdateLocationReq = NonEmpty Waypoint

-- Short field names for lesser json array size:
data Waypoint = Waypoint
  { pt :: LatLong, -- point
    ts :: UTCTime, -- timestamp
    acc :: Maybe Double -- accuracy, optional for now
  }
  deriving (Generic, ToJSON, Show, FromJSON, ToSchema, PrettyShow)

data RideStatus
  = ON_RIDE
  | ON_PICKUP
  | IDLE
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data UpdateLocationHandle m = UpdateLocationHandle
  { driver :: Person.Person,
    findDriverLocation :: m (Maybe DriverLocation),
    upsertDriverLocation :: LatLong -> UTCTime -> Id DM.Merchant -> m (),
    getAssignedRide :: m (Maybe (Id DRide.Ride, DRide.RideStatus)),
    addIntermediateRoutePoints :: Id DRide.Ride -> NonEmpty LatLong -> m ()
  }

data DriverLocationUpdateStreamData = DriverLocationUpdateStreamData
  { rId :: Maybe Text, -- rideId
    mId :: Text, -- merchantId
    ts :: UTCTime, -- timestamp
    st :: UTCTime, -- systemtime when location update recieved
    pt :: LatLong, -- lat log
    acc :: Maybe Double, -- accuracy
    rideStatus :: RideStatus, -- ride status
    da :: Bool, -- driver avaiable
    mode :: Maybe DDInfo.DriverMode
  }
  deriving (Generic, FromJSON, ToJSON)

buildUpdateLocationHandle ::
  Id Person.Person ->
  Flow (UpdateLocationHandle Flow)
buildUpdateLocationHandle driverId = do
  driver <- runInReplica $ QP.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  defaultRideInterpolationHandler <- LocUpd.buildRideInterpolationHandler driver.merchantId False
  pure $
    UpdateLocationHandle
      { driver,
        findDriverLocation = DrLoc.findById driver.merchantId driverId,
        upsertDriverLocation = DrLoc.upsertGpsCoord driverId,
        getAssignedRide = SRide.getInProgressOrNewRideIdAndStatusByDriverId driverId,
        addIntermediateRoutePoints = \rideId ->
          LocUpd.addIntermediateRoutePoints defaultRideInterpolationHandler rideId driverId
      }

streamLocationUpdates ::
  ( MonadFlow m,
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasFlowEnv m r '["driverLocationUpdateTopic" ::: Text]
  ) =>
  Maybe (Id DRide.Ride) ->
  Id DM.Merchant ->
  Id Person.Person ->
  LatLong ->
  UTCTime ->
  Maybe Double ->
  RideStatus ->
  Bool ->
  Maybe DDInfo.DriverMode ->
  m ()
streamLocationUpdates mbRideId merchantId driverId point timestamp accuracy status isDriverActive mbDriverMode = do
  topicName <- asks (.driverLocationUpdateTopic)
  now <- getCurrentTime
  produceMessage
    (topicName, Just (encodeUtf8 $ getId driverId))
    (DriverLocationUpdateStreamData (getId <$> mbRideId) (getId merchantId) timestamp now point accuracy status isDriverActive mbDriverMode)

handleDriverPayments :: (Esq.EsqDBReplicaFlow m r, Esq.EsqDBFlow m r, CacheFlow m r) => Id Person.Person -> Seconds -> m ()
handleDriverPayments driverId diffUtc = do
  now <- getLocalCurrentTime diffUtc
  Redis.whenWithLockRedis (paymentProcessingLockKey driverId.getId) 60 $ do
    ongoingAfterEndTime <- B.runInReplica $ findOngoingAfterEndTime driverId now
    overdueFee <- B.runInReplica $ findOldestFeeByStatus (cast driverId) PAYMENT_OVERDUE

    case (ongoingAfterEndTime, overdueFee) of
      (Nothing, _) -> pure ()
      (Just df, Nothing) -> do
        _ <- updateStatus PAYMENT_PENDING df.id now
        updatePendingPayment True (cast driverId)
      (Just dGFee, Just oDFee) -> mergeDriverFee oDFee dGFee now

    unpaidAfterdeadline <- findUnpaidAfterPayBy driverId now
    case unpaidAfterdeadline of
      Nothing -> pure ()
      Just df -> do
        _ <- updateStatus PAYMENT_OVERDUE df.id now
        QDFS.updateStatus (cast driverId) DDFS.PAYMENT_OVERDUE
        updateSubscription False (cast driverId)

diffTimeMilliseconds :: UTCTime -> UTCTime -> Integer
diffTimeMilliseconds startTime endTime =
  round $ nominalDiffTimeToSeconds (diffUTCTime endTime startTime) * 1000

updateLocationHandler ::
  ( HasFlowEnv m r '["driverLocationUpdateRateLimitOptions" ::: APIRateLimitOptions],
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasFlowEnv m r '["driverLocationUpdateTopic" ::: Text],
    MonadTime m,
    CacheFlow m r,
    EsqDBFlow m r,
    Esq.EsqDBReplicaFlow m r
  ) =>
  UpdateLocationHandle m ->
  UpdateLocationReq ->
  m APISuccess
updateLocationHandler UpdateLocationHandle {..} waypoints = withLogTag "driverLocationUpdate" $
  withLogTag ("driverId-" <> driver.id.getId) $ do
    start <- getCurrentTime
    let driverId = driver.id
    thresholdConfig <- QTConf.findByMerchantId driver.merchantId >>= fromMaybeM (TransporterConfigNotFound driver.merchantId.getId)
    end <- getCurrentTime
    logDebug $ "findByMerchantId : " <> diffTimeMilliseconds start end

    start <- getCurrentTime
    when (thresholdConfig.subscription) $ do
      -- window end time over - still ongoing - sendPaymentReminder
      -- payBy is also over - still ongoing/pending - unsubscribe
      handleDriverPayments driverId thresholdConfig.timeDiffFromUtc
    end <- getCurrentTime
    logDebug $ "handleDriverPayments : " <> diffTimeMilliseconds start end

    start <- getCurrentTime
    driverInfo <- DInfo.findById (cast driverId) >>= fromMaybeM (PersonNotFound driverId.getId)
    end <- getCurrentTime
    logDebug $ "DInfo.findById : " <> diffTimeMilliseconds start end

    when (length waypoints > 100) $ logError $ "way points more then 100 points" <> show (length waypoints) <> " on_ride:" <> show driverInfo.onRide
    logInfo $ "got location updates: " <> getId driverId <> " " <> encodeToText waypoints

    start <- getCurrentTime
    checkLocationUpdatesRateLimit driverId
    end <- getCurrentTime
    logDebug $ "checkLocationUpdatesRateLimit : " <> diffTimeMilliseconds start end

    let minLocationAccuracy = thresholdConfig.minLocationAccuracy
    unless (driver.role == Person.DRIVER) $ throwError AccessDenied
    LocUpd.whenWithLocationUpdatesLock driverId $ do
      start <- getCurrentTime
      mbOldLoc <- findDriverLocation
      end <- getCurrentTime
      logDebug $ "findDriverLocation : " <> diffTimeMilliseconds start end

      start <- getCurrentTime
      let sortedWaypoint = toList $ NE.sortWith (.ts) waypoints
          filteredWaypoint = maybe sortedWaypoint (\oldLoc -> filter ((oldLoc.coordinatesCalculatedAt <) . (.ts)) sortedWaypoint) mbOldLoc
      end <- getCurrentTime
      logDebug $ "filteredWaypoint : " <> diffTimeMilliseconds start end

      start <- getCurrentTime
      mbRideIdAndStatus <- getAssignedRide
      end <- getCurrentTime
      logDebug $ "getAssignedRide : " <> diffTimeMilliseconds start end

      case filteredWaypoint of
        [] -> logWarning "Incoming points are older than current one, ignoring"
        (a : ax) -> do
          let newWaypoints = a :| ax
              currPoint = NE.last newWaypoints
          start <- getCurrentTime
          upsertDriverLocation currPoint.pt currPoint.ts driver.merchantId
          end <- getCurrentTime
          logDebug $ "upsertDriverLocation : " <> diffTimeMilliseconds start end

          fork "updating in kafka" $
            forM_ (a : ax) $ \point -> do
              status <- case mbRideIdAndStatus of
                Just (_, DRide.INPROGRESS) -> pure ON_RIDE
                Just (_, DRide.NEW) -> pure ON_PICKUP
                _ -> pure IDLE
              start <- getCurrentTime
              streamLocationUpdates (fst <$> mbRideIdAndStatus) driver.merchantId driverId point.pt point.ts point.acc status driverInfo.active driverInfo.mode
              end <- getCurrentTime
              logDebug $ "streamLocationUpdates : " <> diffTimeMilliseconds start end

      start <- getCurrentTime
      let filteredWaypointWithAccuracy = filter (\val -> fromMaybe 0.0 val.acc <= minLocationAccuracy) filteredWaypoint
      let filteredNearPoints = maybe filteredWaypointWithAccuracy (\oldLoc -> filter (filterVeryNearPoints thresholdConfig.driverLocationAccuracyBuffer oldLoc) filteredWaypointWithAccuracy) mbOldLoc
      end <- getCurrentTime
      logDebug $ "filteredNearPoints : " <> diffTimeMilliseconds start end

      case filteredNearPoints of
        [] -> logWarning "Accuracy of the points is low, ignoring"
        (a : ax) -> do
          let newWaypoints = a :| ax
          fork "update driver speed in redis" $
            forM_ (a : ax) $ \point -> do
              start
              updateDriverSpeedInRedis driver.merchantId driverId point.pt point.ts
              end "updateDriverSpeedInRedis"

          start <- getCurrentTime
          maybe
            (logInfo "No ride is assigned to driver, ignoring")
            (\(rideId, rideStatus) -> when (rideStatus == DRide.INPROGRESS) $ addIntermediateRoutePoints rideId $ NE.map (.pt) newWaypoints)
            mbRideIdAndStatus
          end <- getCurrentTime
          logDebug $ "addIntermediateRoutePoints : " <> diffTimeMilliseconds start end

    pure Success

checkLocationUpdatesRateLimit ::
  ( Redis.HedisFlow m r,
    HasFlowEnv m r '["driverLocationUpdateRateLimitOptions" ::: APIRateLimitOptions]
  ) =>
  Id Person.Person ->
  m ()
checkLocationUpdatesRateLimit personId = do
  let key = locationUpdatesHitsCountKey personId
  hitsLimit <- asks (.driverLocationUpdateRateLimitOptions.limit)
  limitResetTimeInSec <- asks (.driverLocationUpdateRateLimitOptions.limitResetTimeInSec)
  unlessM (slidingWindowLimiter key hitsLimit limitResetTimeInSec) $ do
    logError "Location updates hitting limit, ignoring"
    throwError $ HitsLimitError limitResetTimeInSec

locationUpdatesHitsCountKey :: Id Person.Person -> Text
locationUpdatesHitsCountKey personId = "BPP:DriverLocationUpdates:" <> getId personId <> ":hitsCount"

filterVeryNearPoints :: Meters -> DriverLocation -> Waypoint -> Bool
filterVeryNearPoints thresholdDistance oldLoc currwpt =
  highPrecMetersToMeters (distanceBetweenInMeters (LatLong oldLoc.lat oldLoc.lon) (currwpt.pt)) > thresholdDistance
