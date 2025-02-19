{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Ride where

import Data.String.Conversions (cs)
import qualified Data.Text as T
import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.Common as SReqD
import qualified Domain.Types.DriverGoHomeRequest as DGetHomeRequest
import qualified Domain.Types.DriverInformation as DDI
import Domain.Types.EmptyDynamicParam
import Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity as DTMM
import Domain.Types.Person
import qualified Domain.Types.Person as DPerson
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.RideDetails as SRD
import qualified Domain.Types.RideRelatedNotificationConfig as DRN
import Domain.Types.SearchRequestForDriver
import qualified Domain.Types.SearchRequestForDriver as SReqD
import Domain.Types.SearchTry
import qualified Domain.Types.ServiceTierType as DST
import qualified Domain.Types.TransporterConfig as DTC
import qualified Domain.Types.Vehicle as DVeh
import Domain.Utils
import Environment
import Kernel.External.Maps (LatLong (..))
import qualified Kernel.External.Maps.Types as Maps
import qualified Kernel.External.Notification as Notification
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.DriverScore as DS
import qualified Lib.DriverScore.Types as DST
import qualified SharedLogic.DriverPool as DP
import qualified SharedLogic.External.LocationTrackingService.Flow as LF
import qualified SharedLogic.ScheduledNotifications as SN
import qualified Storage.CachedQueries.Driver.GoHomeRequest as CQDGR
import qualified Storage.CachedQueries.RideRelatedNotificationConfig as SCRRNC
import qualified Storage.CachedQueries.VehicleServiceTier as CQVST
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.BusinessEvent as QBE
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.DriverQuote as QDQ
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RideDetails as QRideD
import qualified Storage.Queries.RiderDetails as QRiderD
import qualified Storage.Queries.SearchRequestForDriver as QSRD
import Storage.Queries.Vehicle as QVeh
import Storage.Queries.VehicleRegistrationCertificate as QVRC
import Tools.Error
import Tools.Event
import qualified Tools.Notifications as Notify

initializeRide ::
  Merchant ->
  DPerson.Person ->
  DBooking.Booking ->
  Maybe Text ->
  Maybe Bool ->
  Maybe Text ->
  Maybe Bool ->
  Flow (DRide.Ride, SRD.RideDetails, DVeh.Vehicle)
initializeRide merchant driver booking mbOtpCode enableFrequentLocationUpdates mbClientId enableOtpLessRide = do
  let merchantId = merchant.id
  otpCode <-
    case mbOtpCode of
      Just otp -> pure otp
      Nothing -> do
        riderId <- booking.riderId & fromMaybeM (BookingFieldNotPresent "riderId")
        riderDetails <- QRiderD.findById riderId >>= fromMaybeM (RiderDetailsNotFound riderId.getId)
        case riderDetails.otpCode of
          Nothing -> do
            otpCode <- generateOTPCode
            QRiderD.updateOtpCode (Just otpCode) riderDetails.id
            pure otpCode
          Just otp -> pure otp
  ghrId <- CQDGR.setDriverGoHomeIsOnRideStatus driver.id booking.merchantOperatingCityId True
  previousRideInprogress <- bool (QDI.findByPrimaryKey driver.id) (pure Nothing) (booking.isScheduled)
  let isDriverOnRide = bool (Just False) (previousRideInprogress >>= Just . isJust <$> (.driverTripEndLocation)) (isJust previousRideInprogress)
  now <- getCurrentTime
  vehicle <- QVeh.findById driver.id >>= fromMaybeM (VehicleNotFound driver.id.getId)
  ride <- buildRide driver booking ghrId otpCode enableFrequentLocationUpdates mbClientId previousRideInprogress now vehicle merchant.onlinePayment enableOtpLessRide
  rideDetails <- buildRideDetails ride driver vehicle

  QRB.updateStatus booking.id DBooking.TRIP_ASSIGNED
  QRide.createRide ride
  QRideD.create rideDetails
  Redis.withWaitOnLockRedisWithExpiry (isOnRideWithAdvRideConditionKey driver.id.getId) 4 4 $ do
    when (not booking.isScheduled) $ do
      whenJust (booking.toLocation) $ \toLoc -> do
        QDI.updateTripCategoryAndTripEndLocationByDriverId (cast driver.id) (Just ride.tripCategory) (Just (Maps.LatLong toLoc.lat toLoc.lon))
      QDI.updateOnRide True (cast driver.id)
    Redis.unlockRedis (offerQuoteLockKeyWithCoolDown ride.driverId)
    when (isDriverOnRide == Just True) $ QDI.updateHasAdvancedRide (cast ride.driverId) True
    Redis.unlockRedis (editDestinationLockKey ride.driverId)
  unless booking.isScheduled $ void $ LF.rideDetails ride.id DRide.NEW merchantId ride.driverId booking.fromLocation.lat booking.fromLocation.lon (Just ride.isAdvanceBooking) Nothing

  triggerRideCreatedEvent RideEventData {ride = ride, personId = cast driver.id, merchantId = merchantId}
  QBE.logDriverAssignedEvent (cast driver.id) booking.id ride.id booking.distanceUnit

  if booking.isScheduled
    then Notify.driverScheduledRideAcceptanceAlert booking.merchantOperatingCityId Notification.SCHEDULED_RIDE_NOTIFICATION notificationTitle (messageForScheduled booking) driver driver.deviceToken
    else Notify.notifyDriverWithProviders booking.merchantOperatingCityId notificationType notificationTitle (message booking) driver driver.deviceToken EmptyDynamicParam

  fork "DriverScoreEventHandler OnNewRideAssigned" $
    DS.driverScoreEventHandler booking.merchantOperatingCityId DST.OnNewRideAssigned {merchantId = merchantId, driverId = ride.driverId, currency = ride.currency, distanceUnit = booking.distanceUnit}

  notifyRideRelatedNotificationOnEvent ride now DRN.RIDE_ASSIGNED
  notifyRideRelatedNotificationOnEvent ride now DRN.PICKUP_TIME

  return (ride, rideDetails, vehicle)
  where
    notificationType = Notification.DRIVER_ASSIGNMENT
    notificationTitle = "Driver has been assigned the ride!"
    message uBooking =
      cs $
        unwords
          [ "You have been assigned a ride for",
            cs (showTimeIst uBooking.startTime) <> ".",
            "Check the app for more details."
          ]
    messageForScheduled uBooking =
      cs $
        unwords
          [ "You have been assigned a scheduled ride for",
            cs (showTimeIst uBooking.startTime) <> ".",
            "Check the app for more details."
          ]

    notifyRideRelatedNotificationOnEvent ride now timeDiffEvent = do
      rideRelatedNotificationConfigList <- SCRRNC.findAllByMerchantOperatingCityIdAndTimeDiffEventInRideFlow booking.merchantOperatingCityId timeDiffEvent booking.configInExperimentVersions
      forM_ rideRelatedNotificationConfigList (SN.pushReminderUpdatesInScheduler booking ride now driver.id)

buildRideDetails ::
  DRide.Ride ->
  DPerson.Person ->
  DVeh.Vehicle ->
  Flow SRD.RideDetails
buildRideDetails ride driver vehicle = do
  now <- getCurrentTime
  vehicleRegCert <- QVRC.findLastVehicleRCWrapper vehicle.registrationNo
  cityServiceTiers <- CQVST.findAllByMerchantOpCityId ride.merchantOperatingCityId
  let defaultServiceTierName = (.name) <$> find (\vst -> vehicle.variant `elem` vst.defaultForVehicleVariant) cityServiceTiers
  return $
    SRD.RideDetails
      { id = ride.id,
        driverName = driver.firstName,
        driverNumber = driver.mobileNumber,
        driverCountryCode = driver.mobileCountryCode,
        vehicleNumber = vehicle.registrationNo,
        vehicleColor = Just vehicle.color,
        vehicleVariant = Just vehicle.variant,
        vehicleModel = Just vehicle.model,
        vehicleClass = Nothing,
        vehicleAge = getVehicleAge vehicle.mYManufacturing now,
        fleetOwnerId = vehicleRegCert >>= (.fleetOwnerId),
        defaultServiceTierName = defaultServiceTierName,
        createdAt = Just now,
        merchantId = ride.merchantId,
        merchantOperatingCityId = Just ride.merchantOperatingCityId
      }

buildRide ::
  DPerson.Person ->
  DBooking.Booking ->
  Maybe (Id DGetHomeRequest.DriverGoHomeRequest) ->
  Text ->
  Maybe Bool ->
  Maybe Text ->
  Maybe DDI.DriverInformation ->
  UTCTime ->
  DVeh.Vehicle ->
  Bool ->
  Maybe Bool ->
  Flow DRide.Ride
buildRide driver booking ghrId otp enableFrequentLocationUpdates clientId dinfo now vehicle onlinePayment enableOtpLessRide = do
  guid <- Id <$> generateGUID
  shortId <- generateShortId
  deploymentVersion <- asks (.version)
  trackingUrl <- buildTrackingUrl guid
  let previousRideToLocation = dinfo >>= (.driverTripEndLocation)
  let status = bool DRide.NEW DRide.UPCOMING booking.isScheduled
  return
    DRide.Ride
      { id = guid,
        pickupDropOutsideOfThreshold = Nothing,
        bookingId = booking.id,
        clientId = clientId,
        shortId = shortId,
        merchantId = Just booking.providerId,
        merchantOperatingCityId = booking.merchantOperatingCityId,
        status = status,
        driverId = cast driver.id,
        otp = otp,
        endOtp = Nothing,
        trackingUrl = trackingUrl,
        fare = Nothing,
        currency = booking.currency,
        distanceUnit = booking.distanceUnit,
        traveledDistance = 0,
        chargeableDistance = Nothing,
        driverArrivalTime = Nothing,
        tripStartTime = Nothing,
        tripEndTime = Nothing,
        tripStartPos = Nothing,
        tripEndPos = Nothing,
        rideEndedBy = Nothing,
        isDriverSpecialLocWarrior = fromMaybe False (dinfo <&> (.isSpecialLocWarrior)),
        previousRideTripEndPos = LatLong <$> (previousRideToLocation <&> (.lat)) <*> (previousRideToLocation <&> (.lon)),
        previousRideTripEndTime = Nothing,
        isAdvanceBooking = isJust previousRideToLocation,
        startOdometerReading = Nothing,
        endOdometerReading = Nothing,
        fromLocation = booking.fromLocation, --check if correct
        toLocation = booking.toLocation, --check if correct
        stops = booking.stops,
        fareParametersId = Nothing,
        distanceCalculationFailed = Nothing,
        createdAt = now,
        updatedAt = now,
        driverDeviatedToTollRoute = Just False,
        driverDeviatedFromRoute = Just False,
        numberOfSnapToRoadCalls = Nothing,
        numberOfOsrmSnapToRoadCalls = Nothing,
        numberOfSelfTuned = Nothing,
        numberOfDeviation = Nothing,
        tollCharges = Nothing,
        tollNames = Nothing,
        tollConfidence = Nothing,
        estimatedTollCharges = booking.fareParams.tollCharges,
        estimatedTollNames = booking.tollNames,
        uiDistanceCalculationWithAccuracy = Nothing,
        uiDistanceCalculationWithoutAccuracy = Nothing,
        isFreeRide = Just False,
        driverGoHomeRequestId = ghrId,
        safetyAlertTriggered = False,
        enableFrequentLocationUpdates = enableFrequentLocationUpdates,
        vehicleServiceTierSeatingCapacity = booking.vehicleServiceTierSeatingCapacity,
        vehicleServiceTierAirConditioned = booking.vehicleServiceTierAirConditioned,
        isAirConditioned = booking.isAirConditioned,
        clientSdkVersion = driver.clientSdkVersion,
        clientBundleVersion = driver.clientBundleVersion,
        clientDevice = driver.clientDevice,
        clientConfigVersion = driver.clientConfigVersion,
        backendConfigVersion = driver.backendConfigVersion,
        backendAppVersion = Just deploymentVersion.getDeploymentVersion,
        tripCategory = booking.tripCategory,
        vehicleServiceTierName = Just booking.vehicleServiceTierName,
        vehicleVariant = Just $ vehicle.variant,
        onlinePayment = onlinePayment,
        enableOtpLessRide = enableOtpLessRide,
        cancellationFeeIfCancelled = Nothing,
        tipAmount = Nothing,
        passedThroughDestination = Nothing,
        deliveryFileIds = Nothing,
        destinationReachedAt = Nothing,
        estimatedEndTimeRange = Nothing,
        rideTags = Nothing,
        hasStops = booking.hasStops,
        isPickupOrDestinationEdited = Just False
      }

buildTrackingUrl :: Id DRide.Ride -> Flow BaseUrl
buildTrackingUrl rideId = do
  bppUIUrl <- asks (.selfUIUrl)
  let rideid = T.unpack (getId rideId)
  return $
    bppUIUrl
      { --TODO: find a way to build it using existing types from Routes
        baseUrlPath = baseUrlPath bppUIUrl <> "/driver/location/" <> rideid
      }

deactivateExistingQuotes :: Id DTMM.MerchantOperatingCity -> Id Merchant -> Id Person -> Id SearchTry -> Price -> Flow [SearchRequestForDriver]
deactivateExistingQuotes merchantOpCityId merchantId quoteDriverId searchTryId estimatedFare = do
  driverSearchReqs <- QSRD.findAllActiveBySTId searchTryId SReqD.Active
  QDQ.setInactiveBySTId searchTryId
  QSRD.setInactiveBySTId searchTryId
  pullExistingRideRequests merchantOpCityId driverSearchReqs merchantId quoteDriverId estimatedFare
  return driverSearchReqs

pullExistingRideRequests :: Id DTMM.MerchantOperatingCity -> [SearchRequestForDriver] -> Id Merchant -> Id Person -> Price -> Flow ()
pullExistingRideRequests merchantOpCityId driverSearchReqs merchantId quoteDriverId estimatedFare = do
  for_ driverSearchReqs $ \driverReq -> do
    let driverId = driverReq.driverId
    unless (driverId == quoteDriverId) $ do
      DP.decrementTotalQuotesCount merchantId merchantOpCityId (cast driverReq.driverId) driverReq.searchTryId
      DP.removeSearchReqIdFromMap merchantId driverId driverReq.searchTryId
      void $ QSRD.updateDriverResponse (Just SReqD.Pulled) SReqD.Inactive Nothing driverReq.renderedAt driverReq.respondedAt driverReq.id
      driver_ <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
      Notify.notifyDriverClearedFare merchantOpCityId driver_ driverReq.searchTryId estimatedFare

searchRequestKey :: Text -> Text
searchRequestKey sId = "Driver:Search:Request:" <> sId

multipleRouteKey :: Text -> Text
multipleRouteKey id = "multiple-routes-" <> id

confirmLockKey :: Id DBooking.Booking -> Text
confirmLockKey (Id id) = "Driver:Confirm:BookingId-" <> id

bookingRequestKeySoftUpdate :: Text -> Text
bookingRequestKeySoftUpdate bId = "Driver:Booking:Request:SoftUpdate" <> bId

multipleRouteKeySoftUpdate :: Text -> Text
multipleRouteKeySoftUpdate id = "multiple-routes-SoftUpdate-" <> id

isOnRideWithAdvRideConditionKey :: Text -> Text
isOnRideWithAdvRideConditionKey driverId = "Driver:SetOnRide:" <> driverId

lockRide :: Text -> Text
lockRide rideId = "D:C:Rd-" <> rideId

editDestinationLockKey :: Id Person -> Text
editDestinationLockKey driverId = "Driver:EditDes:DId-" <> driverId.getId

editDestinationUpdatedLocGeohashKey :: Id Person -> Text
editDestinationUpdatedLocGeohashKey driverId = "Driver:EditDes:GeoHash:DId-" <> driverId.getId

offerQuoteLockKeyWithCoolDown :: Id Person -> Text
offerQuoteLockKeyWithCoolDown driverId = "Driver:OffQuote:CD:DId-" <> driverId.getId

updateOnRideStatusWithAdvancedRideCheck :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id Person -> Maybe DRide.Ride -> m ()
updateOnRideStatusWithAdvancedRideCheck personId mbRide = do
  lockAcquired <- case mbRide of
    Just ride -> Redis.tryLockRedis (lockRide (ride.id.getId)) 10
    Nothing -> pure True
  if lockAcquired
    then do
      Redis.withWaitOnLockRedisWithExpiry (isOnRideWithAdvRideConditionKey personId.getId) 4 4 $ do
        hasAdvancedRide <- QDI.findById (cast personId) <&> maybe False (.hasAdvanceBooking)
        unless hasAdvancedRide $ QDI.updateOnRideAndTripEndLocationByDriverId (cast personId) False Nothing
        QDI.updateHasAdvancedRide (cast personId) False
        void $ Redis.del $ editDestinationUpdatedLocGeohashKey personId
    else throwError $ DriverTransactionTryAgain (Just personId.getId)

throwErrorOnRide :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Bool -> DDI.DriverInformation -> Bool -> m ()
throwErrorOnRide includeDriverCurrentlyOnRide driverInfo isForwardRequest = do
  let checkOnRide = if includeDriverCurrentlyOnRide && isForwardRequest then driverInfo.hasAdvanceBooking else driverInfo.onRide
  when checkOnRide $ throwError DriverOnRide

calculateEstimatedEndTimeRange :: UTCTime -> Seconds -> Maybe DTC.ArrivalTimeBufferOfVehicle -> DST.ServiceTierType -> Maybe DRide.EstimatedEndTimeRange
calculateEstimatedEndTimeRange currTime tripEstimatedDuration bufferJson serviceTier =
  case getArrivalTimeBufferOfVehicle bufferJson serviceTier of
    Nothing -> Nothing
    Just timebufferOfVehicle ->
      let start = addUTCTime (secondsToNominalDiffTime (tripEstimatedDuration + div timebufferOfVehicle 2)) currTime
          end = addUTCTime (secondsToNominalDiffTime timebufferOfVehicle) start
       in Just $ DRide.EstimatedEndTimeRange {start = start, end = end}

getArrivalTimeBufferOfVehicle :: Maybe DTC.ArrivalTimeBufferOfVehicle -> DST.ServiceTierType -> Maybe Seconds
getArrivalTimeBufferOfVehicle bufferJson serviceTier =
  bufferJson >>= \buffer -> case serviceTier of
    DST.SEDAN -> buffer.sedan
    DST.SUV -> buffer.suv
    DST.HATCHBACK -> buffer.hatchback
    DST.AUTO_RICKSHAW -> buffer.autorickshaw
    DST.BIKE -> buffer.bike
    DST.DELIVERY_BIKE -> buffer.deliverybike
    DST.TAXI -> buffer.taxi
    DST.TAXI_PLUS -> buffer.taxiplus
    DST.PREMIUM_SEDAN -> buffer.premiumsedan
    DST.BLACK -> buffer.black
    DST.BLACK_XL -> buffer.blackxl
    DST.ECO -> buffer.hatchback
    DST.COMFY -> buffer.sedan
    DST.PREMIUM -> buffer.sedan
    DST.AMBULANCE_TAXI -> buffer.ambulance
    DST.AMBULANCE_TAXI_OXY -> buffer.ambulance
    DST.AMBULANCE_AC -> buffer.ambulance
    DST.AMBULANCE_AC_OXY -> buffer.ambulance
    DST.AMBULANCE_VENTILATOR -> buffer.ambulance
    DST.SUV_PLUS -> buffer.suvplus
    DST.HERITAGE_CAB -> buffer.heritagecab
    DST.EV_AUTO_RICKSHAW -> buffer.evautorickshaw
    DST.DELIVERY_LIGHT_GOODS_VEHICLE -> buffer.deliveryLightGoodsVehicle
    DST.DELIVERY_TRUCK_MINI -> buffer.deliveryLightGoodsVehicle
    DST.DELIVERY_TRUCK_SMALL -> buffer.deliveryLightGoodsVehicle
    DST.DELIVERY_TRUCK_MEDIUM -> buffer.deliveryLightGoodsVehicle
    DST.DELIVERY_TRUCK_LARGE -> buffer.deliveryLightGoodsVehicle
    DST.DELIVERY_TRUCK_ULTRA_LARGE -> buffer.deliveryLightGoodsVehicle
    DST.BUS_NON_AC -> buffer.busNonAc
    DST.BUS_AC -> buffer.busAc
