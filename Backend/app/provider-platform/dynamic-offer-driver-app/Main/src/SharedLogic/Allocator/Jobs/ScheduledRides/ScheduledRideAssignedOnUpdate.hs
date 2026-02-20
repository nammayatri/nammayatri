{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Allocator.Jobs.ScheduledRides.ScheduledRideAssignedOnUpdate where

import qualified AWS.S3 as S3
import Control.Lens ((^?), _head)
import qualified Data.HashMap.Strict as HMS
import qualified Data.Map as M
import qualified Domain.Action.UI.Ride.CancelRide as RideCancel
import Domain.Types.CancellationReason as DCR
import qualified Domain.Types.Ride as DRide
import Kernel.Beam.Functions (runInReplica)
import Kernel.External.Maps.HasCoordinates (HasCoordinates (..))
import Kernel.External.Maps.Interface.Types
import Kernel.External.Maps.Types (LatLong (..))
import qualified Kernel.External.Maps.Types as Maps
import Kernel.External.Types
import Kernel.Prelude
import qualified Kernel.Storage.Clickhouse.Config as CH
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (KafkaProducerTools)
import Kernel.Utils.Common
import Lib.Scheduler
import Lib.SessionizerMetrics.Types.Event
import SharedLogic.Allocator
import SharedLogic.CallBAP
import SharedLogic.CallBAPInternal
import qualified SharedLogic.CallInternalMLPricing as ML
import qualified SharedLogic.External.LocationTrackingService.Flow as LF
import qualified SharedLogic.External.LocationTrackingService.Flow as LTF
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import SharedLogic.GoogleTranslate (TranslateFlow)
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.Vehicle as QVeh
import qualified Tools.Maps as TMaps
import qualified Tools.Metrics as Metrics
import TransactionLogs.Types

sendScheduledRideAssignedOnUpdate ::
  ( MonadFlow m,
    EsqDBFlow m r,
    EncFlow m r,
    HasHttpClientOptions r c,
    HasShortDurationRetryCfg r c,
    CacheFlow m r,
    HasField "modelNamesHashMap" r (HMS.HashMap Text Text),
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasField "s3Env" r (S3.S3Env m),
    LT.HasLocationService m r,
    HasFlowEnv m r '["ondcTokenHashMap" ::: HMS.HashMap KeyConfig TokenConfig],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HMS.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    EsqDBReplicaFlow m r,
    HasField "searchRequestExpirationSeconds" r NominalDiffTime,
    HasField "jobInfoMap" r (M.Map Text Bool),
    HasField "maxShards" r Int,
    HasField "schedulerSetName" r Text,
    HasField "schedulerType" r SchedulerType,
    Metrics.HasSendSearchRequestToDriverMetrics m r,
    HasLongDurationRetryCfg r c,
    HasField "singleBatchProcessingTempDelay" r NominalDiffTime,
    TranslateFlow m r,
    HasFlowEnv m r '["maxNotificationShards" ::: Int],
    Redis.HedisFlow m r,
    EventStreamFlow m r,
    Metrics.HasCoreMetrics r,
    HasField "enableAPILatencyLogging" r Bool,
    HasField "enableAPIPrometheusMetricLogging" r Bool,
    HasFlowEnv m r '["appBackendBapInternal" ::: AppBackendBapInternal],
    HasFlowEnv m r '["mlPricingInternal" ::: ML.MLPricingInternal],
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv,
    HasField "blackListedJobs" r [Text]
  ) =>
  Job 'ScheduledRideAssignedOnUpdate ->
  m ExecutionResult
sendScheduledRideAssignedOnUpdate Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
      bookingId = jobData.bookingId
      driverId = jobData.driverId
      rideId = jobData.rideId
  mbRide <- QRide.findById rideId
  mbDriverInfo <- QDI.findById driverId
  case (mbRide, mbDriverInfo) of
    (Nothing, _) -> do
      return $ Terminate "Job Terminated because Ride not found"
    (Just ride, Nothing) -> do
      let cReason = "Ride is Reallocated driverInfo not found"
      cancelOrReallocate ride cReason True (RideCancel.ApplicationRequestorId id.getId)
      return $ Terminate "Job is Terminated and Ride is Reallocated driverInfo not found"
    (Just ride, Just driverInfo) -> do
      case (driverInfo.onRide, ride.status, driverInfo.active) of
        (_, _, False) -> do
          let cReason = "Ride is Reallocated because driver is not active"
          cancelOrReallocate ride cReason True (RideCancel.ApplicationRequestorId id.getId)
          return $ Terminate "Job is Terminated and Ride is Reallocated because driver is not active"
        (False, DRide.UPCOMING, _) -> do
          mbDriver <- runInReplica $ QP.findById driverId
          mbBooking <- QBooking.findById bookingId
          mbVehicle <- runInReplica $ QVeh.findById driverId
          mbtransporterConfig <- SCTC.findByMerchantOpCityId ride.merchantOperatingCityId Nothing
          let mbScheduledPickupTime = driverInfo.latestScheduledBooking
          case (mbDriver, mbVehicle, mbBooking, mbtransporterConfig, mbScheduledPickupTime) of
            (Just driver, Just vehicle, Just booking, Just transporterConfig, Just scheduledPickupTime) -> do
              let fromLocation = booking.fromLocation
                  pickupLoc = LatLong {lat = fromLocation.lat, lon = fromLocation.lon}
                  merchantId = booking.providerId
                  merchantOperatingCityId = booking.merchantOperatingCityId
              mbCurrentDriverLocation <- do
                driverLocations <- withTryCatch "driversLocation:callPayout" $ LTF.driversLocation [driverId]
                case driverLocations of
                  Left _err -> do
                    return Nothing
                  Right locations -> return $ locations ^? _head
              case mbCurrentDriverLocation of
                (Just dloc) -> do
                  let currentDriverLocation = LatLong {lat = dloc.lat, lon = dloc.lon}
                  let req1 =
                        TMaps.GetDistanceReq
                          { origin = currentDriverLocation,
                            destination = pickupLoc,
                            travelMode = Just TMaps.CAR,
                            distanceUnit = Meter,
                            sourceDestinationMapping = Nothing
                          }
                  responseArray <- errorCatchAndHandle [req1] (TMaps.getDistanceForScheduledRides merchantId ride.merchantOperatingCityId (Just ride.id.getId))
                  if isAPIError responseArray
                    then do
                      let cReason = "Ride is Reallocated due to getDistance API failure"
                      cancelOrReallocate ride cReason True (RideCancel.ApplicationRequestorId id.getId)
                      return $ Terminate "Job is Terminated and Ride is Reallocated due to getDistance API failure"
                    else do
                      -- let sumOfDistances = sumDistances responseArray
                      -- isDriverTooFar <- isDriverTooFarFromPickup transporterConfig vehicle estimatedDistinKm scheduledPickupTime
                      let sumOfDurations = sumDuration responseArray
                      isDriverTooFar <- isDriverTooFarFromScheduledPickup transporterConfig sumOfDurations scheduledPickupTime
                      if isDriverTooFar
                        then do
                          let cReason = "Ride is Cancelled because driver can't reach pickup of its scheduled booking on time"
                          cancelOrReallocate ride cReason True (RideCancel.MerchantRequestorId (merchantId, merchantOperatingCityId))
                          return $ Terminate "Job is Terminated and Ride is Reallocated because driver can't reach pickup of its scheduled booking on time."
                        else do
                          void $ QDI.updateOnRideAndLatestScheduledBookingAndPickup True Nothing Nothing driverId
                          whenJust (booking.toLocation) $ \toLoc -> do
                            QDI.updateTripCategoryAndTripEndLocationByDriverId driverId (Just ride.tripCategory) (Just (Maps.LatLong toLoc.lat toLoc.lon))
                          void $ QRide.updateStatus ride.id DRide.NEW
                          void $ LF.rideDetails ride.id DRide.NEW booking.providerId ride.driverId booking.fromLocation.lat booking.fromLocation.lon (Just ride.isAdvanceBooking) (Just $ (LT.Car $ LT.CarRideInfo {pickupLocation = LatLong (booking.fromLocation.lat) (booking.fromLocation.lon), minDistanceBetweenTwoPoints = Nothing, rideStops = Just $ map (\stop -> LatLong stop.lat stop.lon) booking.stops}))
                          void $ sendRideAssignedUpdateToBAP booking ride driver vehicle True -- TODO: handle error
                          return Complete
                _ -> do
                  let cReason = "Ride is Reallocated current driver location not found"
                  cancelOrReallocate ride cReason True (RideCancel.ApplicationRequestorId id.getId)
                  return $ Terminate "Job is Terminated and Ride is Reallocated current driver location not found"
            (_, _, _, _, _) -> do
              let cReason = "Ride is Reallocated driver/vehicle/booking/latestScheduledPickup/transporterConfig not found"
              cancelOrReallocate ride cReason True (RideCancel.ApplicationRequestorId id.getId)
              return $ Terminate "Job is Terminated and Ride is Reallocated driver/vehicle/booking/latestScheduledPickup/transporterConfig is Nothing."
        (True, DRide.UPCOMING, _) -> do
          now <- getCurrentTime
          mbActiveRide <- QRide.getActiveByDriverId driverId
          mbVehicle <- runInReplica $ QVeh.findById driverId
          mbtransporterConfig <- SCTC.findByMerchantOpCityId ride.merchantOperatingCityId Nothing
          result <- runMaybeT $ do
            activeRide <- MaybeT $ return mbActiveRide
            scheduledPickup <- MaybeT $ return (driverInfo.latestScheduledPickup)
            scheduledPickupTime <- MaybeT $ return (driverInfo.latestScheduledBooking)
            merchantId <- MaybeT $ return (ride.merchantId)
            dropLoc' <- MaybeT $ return (activeRide.toLocation)
            transporterConfig <- MaybeT $ return mbtransporterConfig
            vehicle <- MaybeT $ return mbVehicle
            let dropLoc = LatLong {lat = dropLoc'.lat, lon = dropLoc'.lon}
            return (dropLoc, merchantId, scheduledPickup, transporterConfig, vehicle, scheduledPickupTime)
          case result of
            Nothing -> do
              let cReason = "Ride is Reallocated because any one of the above values are Nothing."
              cancelOrReallocate ride cReason True (RideCancel.ApplicationRequestorId id.getId)
              return $ Terminate "Job is Terminated and Ride is Reallocated because any one of the above values are Nothing"
            Just (dropLoc, merchantId, scheduledPickup, transporterConfig, _vehicle, scheduledPickupTime) -> do
              mbCurrentDriverLocation <- do
                driverLocations <- withTryCatch "driversLocation:sendScheduledRideAssignedOnUpdate" $ LTF.driversLocation [driverId]
                case driverLocations of
                  Left _err -> do
                    return Nothing
                  Right locations -> return $ locations ^? _head
              case mbCurrentDriverLocation of
                Just dloc -> do
                  let req1 =
                        TMaps.GetDistanceReq
                          { origin = LatLong {lat = dloc.lat, lon = dloc.lon},
                            destination = dropLoc,
                            travelMode = Just TMaps.CAR,
                            distanceUnit = Meter,
                            sourceDestinationMapping = Nothing
                          }
                  let req2 =
                        TMaps.GetDistanceReq
                          { origin = dropLoc,
                            destination = scheduledPickup,
                            travelMode = Just TMaps.CAR,
                            distanceUnit = Meter,
                            sourceDestinationMapping = Nothing
                          }
                  responseArray <- errorCatchAndHandle [req1, req2] (TMaps.getDistanceForScheduledRides merchantId ride.merchantOperatingCityId (Just ride.id.getId))
                  if isAPIError responseArray
                    then do
                      let cReason = "Ride is Reallocated due to getDistance API failure"
                      cancelOrReallocate ride cReason True (RideCancel.MerchantRequestorId (merchantId, ride.merchantOperatingCityId))
                      return $ Terminate "Job is Terminated and Ride is Reallocated due to getDistance API failure"
                    else do
                      -- let sumOfDistances = sumDistances responseArray
                      -- isDriverTooFar <- isDriverTooFarFromPickup transporterConfig vehicle estimatedDistinKm scheduledPickupTime
                      let sumOfDurations = sumDuration responseArray
                      isDriverTooFar <- isDriverTooFarFromScheduledPickup transporterConfig sumOfDurations scheduledPickupTime
                      if isDriverTooFar
                        then do
                          let cReason = "Ride is Cancelled because driver can't reach pickup of its scheduled booking on time"
                          cancelOrReallocate ride cReason True (RideCancel.MerchantRequestorId (merchantId, ride.merchantOperatingCityId))
                          return $ Terminate "Job is Terminated and Ride is Reallocated because driver can't reach pickup of its scheduled booking on time"
                        else do
                          let rescheduleTime = addUTCTime (transporterConfig.scheduledRideJobRescheduleTime) now
                          return $ ReSchedule rescheduleTime
                Nothing -> do
                  let cReason = "Ride is Reallocated current driver location not found"
                  cancelOrReallocate ride cReason True (RideCancel.MerchantRequestorId (merchantId, ride.merchantOperatingCityId))
                  return $ Terminate "Job is Terminated and Ride is Reallocated current driver location not found"
        (_, _, _) -> do
          return $ Terminate "Job is terminated due to invalid ride status "
  where
    isAPIError [] = False
    isAPIError (APIFailed : _) = True
    isAPIError (_ : xs) = isAPIError xs

    sumDuration :: [Result a b] -> Seconds
    sumDuration = foldr accumulate 0
      where
        accumulate (DistanceResp resp) acc = acc + resp.duration
        accumulate APIFailed acc = acc

    isDriverTooFarFromScheduledPickup transporterConfig estimatedDurationSeconds scheduledPickupTime = do
      now <- getCurrentTime
      let expectedEndTime = addUTCTime (secondsToNominalDiffTime estimatedDurationSeconds) now
      let scheduledPickupTimeWithGraceTime = addUTCTime (transporterConfig.graceTimeForScheduledRidePickup) scheduledPickupTime
      return $ expectedEndTime > scheduledPickupTimeWithGraceTime

cancelOrReallocate ::
  ( MonadFlow m,
    EsqDBFlow m r,
    EncFlow m r,
    HasHttpClientOptions r c,
    HasShortDurationRetryCfg r c,
    CacheFlow m r,
    HasField "modelNamesHashMap" r (HMS.HashMap Text Text),
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasField "s3Env" r (S3.S3Env m),
    LT.HasLocationService m r,
    HasFlowEnv m r '["ondcTokenHashMap" ::: HMS.HashMap KeyConfig TokenConfig],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HMS.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    EsqDBReplicaFlow m r,
    HasField "searchRequestExpirationSeconds" r NominalDiffTime,
    HasField "jobInfoMap" r (M.Map Text Bool),
    HasField "maxShards" r Int,
    HasField "schedulerSetName" r Text,
    HasField "schedulerType" r SchedulerType,
    Metrics.HasSendSearchRequestToDriverMetrics m r,
    HasLongDurationRetryCfg r c,
    HasField "singleBatchProcessingTempDelay" r NominalDiffTime,
    TranslateFlow m r,
    HasFlowEnv m r '["maxNotificationShards" ::: Int],
    Redis.HedisFlow m r,
    EventStreamFlow m r,
    Metrics.HasCoreMetrics r,
    HasField "enableAPILatencyLogging" r Bool,
    HasField "enableAPIPrometheusMetricLogging" r Bool,
    HasFlowEnv m r '["appBackendBapInternal" ::: AppBackendBapInternal],
    HasFlowEnv m r '["mlPricingInternal" ::: ML.MLPricingInternal],
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv,
    HasField "blackListedJobs" r [Text]
  ) =>
  DRide.Ride ->
  Text ->
  Bool ->
  RideCancel.RequestorId ->
  m ()
cancelOrReallocate ride cReason isForceReallocation req = do
  let cancelReq =
        RideCancel.CancelRideReq
          { reasonCode = DCR.CancellationReasonCode cReason,
            additionalInfo = Nothing,
            doCancellationRateBasedBlocking = Nothing
          }
  (_cancellationCnt, _isGoToDisabled) <- RideCancel.cancelRideImpl RideCancel.cancelRideHandle req ride.id cancelReq isForceReallocation
  pure ()

data Result a b = APIFailed | DistanceResp (GetDistanceResp a b)

errorCatchAndHandle ::
  ( ServiceFlow m r,
    HasCoordinates a,
    HasCoordinates b
  ) =>
  [TMaps.GetDistanceReq a b] ->
  ( TMaps.GetDistanceReq a b ->
    m (GetDistanceResp a b)
  ) ->
  m [Result a b]
errorCatchAndHandle reqs func = processRequests reqs
  where
    processRequests [] = return []
    processRequests (req : rest) = do
      resp <- withTryCatch "DistanceResp:errorCatchAndHandle" $ func req
      case resp of
        Left _ -> return [APIFailed]
        Right result -> do
          restResults <- processRequests rest
          return (DistanceResp result : restResults)

sumDistances :: [Result a b] -> Meters
sumDistances = foldr accumulate 0
  where
    accumulate (DistanceResp resp) acc = acc + resp.distance
    accumulate APIFailed acc = acc
