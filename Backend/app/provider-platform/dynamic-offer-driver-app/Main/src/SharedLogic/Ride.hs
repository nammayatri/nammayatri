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
import qualified Domain.Types.Driver.GoHomeFeature.DriverGoHomeRequest as DGetHomeRequest
import Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantOperatingCity as DTMM
import Domain.Types.Person
import qualified Domain.Types.Person as DPerson
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.RideDetails as SRD
import Domain.Types.RideRoute
import Domain.Types.SearchRequestForDriver
import qualified Domain.Types.SearchRequestForDriver as SReqD
import Domain.Types.SearchTry
import qualified Domain.Types.Vehicle as DVeh
import Environment
import qualified Kernel.External.Notification.FCM.Types as FCM
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.DriverScore as DS
import qualified Lib.DriverScore.Types as DST
import qualified SharedLogic.DriverPool as DP
import qualified SharedLogic.External.LocationTrackingService.Flow as LF
import qualified Storage.CachedQueries.Driver.GoHomeRequest as CQDGR
import qualified Storage.CachedQueries.Merchant.TransporterConfig as TC
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.BusinessEvent as QBE
import qualified Storage.Queries.DriverInformation as QDI
import Storage.Queries.DriverOnboarding.VehicleRegistrationCertificate as QVRC
import qualified Storage.Queries.DriverQuote as QDQ
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RideDetails as QRideD
import qualified Storage.Queries.RiderDetails as QRiderD
import qualified Storage.Queries.SearchRequestForDriver as QSRD
import Storage.Queries.Vehicle as QVeh
import Tools.Error
import Tools.Event
import qualified Tools.Notifications as Notify

initializeRide ::
  Id Merchant ->
  DPerson.Person ->
  DBooking.Booking ->
  Maybe Text ->
  Text ->
  Flow (DRide.Ride, SRD.RideDetails, DVeh.Vehicle)
initializeRide merchantId driver booking mbOtpCode routeInfoId = do
  otpCode <-
    case mbOtpCode of
      Just otp -> pure otp
      Nothing -> do
        riderId <- booking.riderId & fromMaybeM (BookingFieldNotPresent "riderId")
        riderDetails <- QRiderD.findById riderId >>= fromMaybeM (RiderDetailsNotFound riderId.getId)
        case riderDetails.otpCode of
          Nothing -> do
            otpCode <- generateOTPCode
            QRiderD.updateOtpCode riderDetails.id otpCode
            pure otpCode
          Just otp -> pure otp
  ghrId <- CQDGR.setDriverGoHomeIsOnRideStatus driver.id booking.merchantOperatingCityId True

  ride <- buildRide driver.id booking ghrId otpCode
  vehicle <- QVeh.findById ride.driverId >>= fromMaybeM (VehicleNotFound ride.driverId.getId)
  rideDetails <- buildRideDetails ride driver vehicle

  QRB.updateStatus booking.id DBooking.TRIP_ASSIGNED
  QRide.createRide ride
  QRideD.create rideDetails
  QDI.updateOnRide (cast driver.id) True
  void $ LF.rideDetails ride.id DRide.NEW merchantId ride.driverId booking.fromLocation.lat booking.fromLocation.lon

  routeInfo :: Maybe RouteInfo <- Redis.safeGet (searchRequestKey routeInfoId)
  case routeInfo of
    Just route -> Redis.setExp (searchRequestKey $ getId ride.id) route 14400
    Nothing -> logDebug "Unable to get the key"

  triggerRideCreatedEvent RideEventData {ride = ride, personId = cast driver.id, merchantId = merchantId}
  QBE.logDriverAssignedEvent (cast driver.id) booking.id ride.id

  Notify.notifyDriver booking.merchantOperatingCityId notificationType notificationTitle (message booking) driver.id driver.deviceToken

  fork "DriverScoreEventHandler OnNewRideAssigned" $
    DS.driverScoreEventHandler booking.merchantOperatingCityId DST.OnNewRideAssigned {merchantId = merchantId, driverId = ride.driverId}

  return (ride, rideDetails, vehicle)
  where
    notificationType = FCM.DRIVER_ASSIGNMENT
    notificationTitle = "Driver has been assigned the ride!"
    message uBooking =
      cs $
        unwords
          [ "You have been assigned a ride for",
            cs (showTimeIst uBooking.startTime) <> ".",
            "Check the app for more details."
          ]

buildRideDetails ::
  DRide.Ride ->
  DPerson.Person ->
  DVeh.Vehicle ->
  Flow SRD.RideDetails
buildRideDetails ride driver vehicle = do
  vehicleRegCert <- QVRC.findLastVehicleRCWrapper vehicle.registrationNo
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
        fleetOwnerId = vehicleRegCert >>= (.fleetOwnerId)
      }

buildRide :: Id Person -> DBooking.Booking -> Maybe (Id DGetHomeRequest.DriverGoHomeRequest) -> Text -> Flow DRide.Ride
buildRide driverId booking ghrId otp = do
  guid <- Id <$> generateGUID
  shortId <- generateShortId
  now <- getCurrentTime
  transporterConfig <- TC.findByMerchantOpCityId booking.merchantOperatingCityId >>= fromMaybeM (TransporterConfigNotFound booking.merchantOperatingCityId.getId)
  trackingUrl <- buildTrackingUrl guid
  return
    DRide.Ride
      { id = guid,
        pickupDropOutsideOfThreshold = Nothing,
        bookingId = booking.id,
        shortId = shortId,
        merchantId = Just booking.providerId,
        merchantOperatingCityId = booking.merchantOperatingCityId,
        status = DRide.NEW,
        driverId = cast driverId,
        otp = otp,
        trackingUrl = trackingUrl,
        fare = Nothing,
        traveledDistance = 0,
        chargeableDistance = Nothing,
        driverArrivalTime = Nothing,
        tripStartTime = Nothing,
        tripEndTime = Nothing,
        tripStartPos = Nothing,
        tripEndPos = Nothing,
        fromLocation = booking.fromLocation, --check if correct
        toLocation = booking.toLocation, --check if correct
        fareParametersId = Nothing,
        distanceCalculationFailed = Nothing,
        createdAt = now,
        updatedAt = now,
        driverDeviatedFromRoute = Just False,
        numberOfSnapToRoadCalls = Nothing,
        numberOfOsrmSnapToRoadCalls = Nothing,
        numberOfDeviation = Nothing,
        uiDistanceCalculationWithAccuracy = Nothing,
        uiDistanceCalculationWithoutAccuracy = Nothing,
        isFreeRide = Just ((getId driverId) `elem` transporterConfig.specialDrivers),
        driverGoHomeRequestId = ghrId,
        safetyAlertTriggered = False
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

deactivateExistingQuotes :: Id DTMM.MerchantOperatingCity -> Id Merchant -> Id Person -> Id SearchTry -> Money -> Flow [SearchRequestForDriver]
deactivateExistingQuotes merchantOpCityId merchantId quoteDriverId searchTryId estimatedFare = do
  driverSearchReqs <- QSRD.findAllActiveBySTId searchTryId
  QDQ.setInactiveBySTId searchTryId
  QSRD.setInactiveBySTId searchTryId
  pullExistingRideRequests merchantOpCityId driverSearchReqs merchantId quoteDriverId estimatedFare
  return driverSearchReqs

pullExistingRideRequests :: Id DTMM.MerchantOperatingCity -> [SearchRequestForDriver] -> Id Merchant -> Id Person -> Money -> Flow ()
pullExistingRideRequests merchantOpCityId driverSearchReqs merchantId quoteDriverId estimatedFare = do
  for_ driverSearchReqs $ \driverReq -> do
    let driverId = driverReq.driverId
    unless (driverId == quoteDriverId) $ do
      DP.decrementTotalQuotesCount merchantId merchantOpCityId (cast driverReq.driverId) driverReq.searchTryId
      DP.removeSearchReqIdFromMap merchantId driverId driverReq.searchTryId
      void $ QSRD.updateDriverResponse driverReq.id SReqD.Pulled
      driver_ <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
      Notify.notifyDriverClearedFare merchantOpCityId driverId driverReq.searchTryId estimatedFare driver_.deviceToken

searchRequestKey :: Text -> Text
searchRequestKey sId = "Driver:Search:Request:" <> sId
