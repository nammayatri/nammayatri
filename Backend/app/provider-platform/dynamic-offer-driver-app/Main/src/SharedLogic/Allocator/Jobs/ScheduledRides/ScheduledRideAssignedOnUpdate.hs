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
import qualified Data.HashMap.Strict as HMS
import qualified Data.Map as M
import qualified Domain.Action.UI.Ride.CancelRide as RideCancel
import Domain.Types.CancellationReason as DCR
import qualified Domain.Types.Ride as DRide
import Kernel.Beam.Functions (runInReplica)
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (KafkaProducerTools)
import Kernel.Utils.Common
import Lib.Scheduler
import Lib.SessionizerMetrics.Types.Event
import SharedLogic.Allocator
import SharedLogic.CallBAP
import SharedLogic.DriverPool as SDP
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
    Metrics.HasCoreMetrics r
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
          case (mbDriver, mbVehicle, mbBooking) of
            (Just driver, Just vehicle, Just booking) -> do
              let fromLocation = booking.fromLocation
                  pickupLoc = LatLong {lat = fromLocation.lat, lon = fromLocation.lon}
                  merchantId = booking.providerId
                  merchantOperatingCityId = booking.merchantOperatingCityId
              currentDriverLocation' <- LTF.driversLocation [driverId]
              mbtransporterConfig <- SCTC.findByMerchantOpCityId ride.merchantOperatingCityId Nothing
              let isAllJust = isJust driverInfo.latestScheduledBooking && isJust (mbtransporterConfig >>= (.avgSpeedOfVehicle))
              case (listToMaybe currentDriverLocation', isAllJust) of
                (Just dloc, True) -> do
                  let currentDriverLocation = LatLong {lat = dloc.lat, lon = dloc.lon}
                  currentLocationtoPickupDistance' <-
                    TMaps.getDistanceForScheduledRides merchantId merchantOperatingCityId $
                      TMaps.GetDistanceReq
                        { origin = currentDriverLocation,
                          destination = pickupLoc,
                          travelMode = Just TMaps.CAR,
                          distanceUnit = Meter,
                          sourceDestinationMapping = Nothing
                        }
                  let estimatedDistinKm = metersToKilometers currentLocationtoPickupDistance'.distance
                  isDriverTooFar <- isDriverTooFarFromPickup mbtransporterConfig mbVehicle estimatedDistinKm driverInfo
                  if isDriverTooFar
                    then do
                      let cReason = "Ride is Cancelled because driver can't reach pickup of its scheduled booking on time"
                      cancelOrReallocate ride cReason True (RideCancel.MerchantRequestorId (merchantId, merchantOperatingCityId))
                      return $ Terminate "Job is Terminated and Ride is Reallocated because driver can't reach pickup of its scheduled booking on time"
                    else do
                      void $ QDI.updateOnRideAndLatestScheduledBookingAndPickup True Nothing Nothing driverId
                      void $ QRide.updateStatus ride.id DRide.NEW
                      void $ LF.rideDetails ride.id DRide.NEW booking.providerId ride.driverId booking.fromLocation.lat booking.fromLocation.lon
                      void $ sendRideAssignedUpdateToBAP booking ride driver vehicle True -- TODO: handle error
                      return Complete
                _ -> do
                  let cReason = "Ride is Reallocated transporterConfig  Or driverInfo.latestScheduledBooking is Nothing"
                  cancelOrReallocate ride cReason True (RideCancel.ApplicationRequestorId id.getId)
                  return $ Terminate "Job is Terminated and Ride is Reallocated transporterConfig  Or driverInfo.latestScheduledBooking is Nothing"
            (_, _, _) -> do
              let cReason = "Ride is Reallocated driver/vehicle/booking not found"
              cancelOrReallocate ride cReason True (RideCancel.ApplicationRequestorId id.getId)
              return $ Terminate "Job is Terminated and Ride is Reallocated driver/vehicle/booking not found"
        (True, DRide.UPCOMING, _) -> do
          now <- getCurrentTime
          mbActiveRide <- QRide.getActiveByDriverId driverId
          mbVehicle <- runInReplica $ QVeh.findById driverId
          mbtransporterConfig <- SCTC.findByMerchantOpCityId ride.merchantOperatingCityId Nothing
          let errorFree = isJust mbActiveRide && isJust driverInfo.latestScheduledPickup && isJust ride.merchantId && isJust driverInfo.latestScheduledBooking && isJust mbVehicle
          let checkTransporterAndAvgSpeed = maybe False (\t -> isJust (t.avgSpeedOfVehicle)) mbtransporterConfig
          let checkToLocation = maybe False (\t -> isJust (t.toLocation)) mbActiveRide
          ( if errorFree && checkTransporterAndAvgSpeed && checkToLocation
              then
                ( do
                    let activeRide = fromJust mbActiveRide
                        scheduledPickup = fromJust driverInfo.latestScheduledPickup
                        merchantId = fromJust ride.merchantId
                        merchantOperatingCityId = ride.merchantOperatingCityId
                        dropLoc' = fromJust activeRide.toLocation
                        dropLoc = LatLong {lat = dropLoc'.lat, lon = dropLoc'.lon}
                    currentDriverLocation' <- LTF.driversLocation [driverId]
                    currentLocationtoDropDistance <- do
                      case listToMaybe currentDriverLocation' of
                        Just dloc -> do
                          let currentDriverLocation = LatLong {lat = dloc.lat, lon = dloc.lon}
                          currentLocationtoDropDistance' <-
                            TMaps.getDistanceForScheduledRides merchantId merchantOperatingCityId $
                              TMaps.GetDistanceReq
                                { origin = currentDriverLocation,
                                  destination = dropLoc,
                                  travelMode = Just TMaps.CAR,
                                  distanceUnit = Meter,
                                  sourceDestinationMapping = Nothing
                                }
                          return currentLocationtoDropDistance'.distance
                        _ -> return 0
                    currentDroptoScheduledPickupDistance <-
                      TMaps.getDistanceForScheduledRides merchantId merchantOperatingCityId $
                        TMaps.GetDistanceReq
                          { origin = dropLoc,
                            destination = scheduledPickup,
                            travelMode = Just TMaps.CAR,
                            distanceUnit = Meter,
                            sourceDestinationMapping = Nothing
                          }
                    let estimatedDistinKm = metersToKilometers (currentLocationtoDropDistance + currentDroptoScheduledPickupDistance.distance)
                    isDriverTooFar <- isDriverTooFarFromPickup mbtransporterConfig mbVehicle estimatedDistinKm driverInfo
                    if isDriverTooFar
                      then do
                        let cReason = "Ride is Cancelled because driver can't reach pickup of its scheduled booking on time"
                        cancelOrReallocate ride cReason True (RideCancel.MerchantRequestorId (merchantId, merchantOperatingCityId))
                        return $ Terminate "Job is Terminated and Ride is Reallocated because driver can't reach pickup of its scheduled booking on time"
                      else do
                        let transporterConfig = fromJust mbtransporterConfig
                            rescheduleTime = addUTCTime (transporterConfig.scheduledRideJobRescheduleTime) now
                        return $ ReSchedule rescheduleTime
                )
              else
                ( do
                    let cReason = "Ride is Reallocated because errorFree && checkTransporterAndAvgSpeed && checkToLocation is False"
                    cancelOrReallocate ride cReason True (RideCancel.ApplicationRequestorId id.getId)
                    return $ Terminate "Job is Terminated and Ride is Reallocated because errorFree && checkTransporterAndAvgSpeed && checkToLocation is False"
                )
            )
        (_, _, _) -> do
          return $ Terminate "Job is terminated due to invalid ride status "
  where
    isDriverTooFarFromPickup mbtransporterConfig mbVehicle estimatedDistinKm driverInfo = do
      now <- getCurrentTime
      let transporterConfig = fromJust mbtransporterConfig
          vehicle = fromJust mbVehicle
          avgSpeeds = fromJust transporterConfig.avgSpeedOfVehicle
          avgSpeedOfVehicleInKM = SDP.getVehicleAvgSpeed vehicle.variant avgSpeeds
          estimatedDistKmDouble = fromIntegral (getKilometers estimatedDistinKm) :: Double
          avgSpeedKmPerHrDouble = fromIntegral (getKilometers avgSpeedOfVehicleInKM) :: Double
          totalTimeinHr = estimatedDistKmDouble / avgSpeedKmPerHrDouble
          totalTimeInSeconds = realToFrac (totalTimeinHr * 3600) :: NominalDiffTime
          expectedEndTime = addUTCTime totalTimeInSeconds now
          scheduledPickupTime = fromJust driverInfo.latestScheduledBooking
          scheduledPickupTimeWithGraceTime = addUTCTime (transporterConfig.graceTimeForScheduledRidePickup) scheduledPickupTime
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
    Metrics.HasCoreMetrics r
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
