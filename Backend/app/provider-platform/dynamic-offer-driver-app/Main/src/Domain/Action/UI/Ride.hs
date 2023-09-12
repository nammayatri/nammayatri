{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Domain.Action.UI.Ride
  ( DriverRideRes (..),
    DriverRideListRes (..),
    OTPRideReq (..),
    listDriverRides,
    arrivedAtPickup,
    otpRideCreate,
  )
where

import Data.String.Conversions
import qualified Data.Text as T
import Data.Time (Day)
import qualified Domain.Action.Beckn.Confirm as DConfirm
import qualified Domain.Action.Beckn.Search as BS
import qualified Domain.Types.BapMetadata as DSM
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.Booking.BookingLocation as DBLoc
import qualified Domain.Types.Driver.DriverFlowStatus as DDFS
import qualified Domain.Types.Driver.GoHomeFeature.DriverGoHomeRequest as DDGR
import qualified Domain.Types.Exophone as DExophone
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Rating as DRating
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.RideDetails as DRD
import qualified Domain.Types.RideDetails as RD
import qualified Domain.Types.RideRoute as RI
import qualified Domain.Types.Vehicle as DVeh
import Environment
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.External.Maps (HasCoordinates (getCoordinates))
import Kernel.External.Maps.Types
import qualified Kernel.External.Notification.FCM.Types as FCM
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Kernel.Utils.Common
import Kernel.Utils.Error.BaseError.HTTPError.BecknAPIError
import qualified Lib.DriverScore as DS
import qualified Lib.DriverScore.Types as DST
import qualified SharedLogic.CallBAP as BP
import qualified SharedLogic.DriverLocation as DLoc
import qualified SharedLogic.External.LocationTrackingService.Flow as LF
import SharedLogic.FareCalculator (fareSum)
import qualified Storage.CachedQueries.BapMetadata as CQSM
import qualified Storage.CachedQueries.Driver.GoHomeRequest as CGHR
import qualified Storage.CachedQueries.Exophone as CQExophone
import Storage.CachedQueries.Merchant as QM
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.BusinessEvent as QBE
import qualified Storage.Queries.Driver.DriverFlowStatus as QDFS
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.Rating as QR
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RideDetails as QRD
import qualified Storage.Queries.RideDetails as QRideD
import Storage.Queries.Vehicle as QVeh
import Tools.Error
import Tools.Event
import qualified Tools.Notifications as Notify

data DriverRideRes = DriverRideRes
  { id :: Id DRide.Ride,
    shortRideId :: ShortId DRide.Ride,
    status :: DRide.RideStatus,
    fromLocation :: DBLoc.BookingLocationAPIEntity,
    toLocation :: DBLoc.BookingLocationAPIEntity,
    driverName :: Text,
    driverNumber :: Maybe Text,
    vehicleVariant :: DVeh.Variant,
    pickupDropOutsideOfThreshold :: Maybe Bool,
    vehicleModel :: Text,
    vehicleColor :: Text,
    vehicleNumber :: Text,
    computedFare :: Maybe Money,
    estimatedBaseFare :: Money,
    estimatedDistance :: Meters,
    driverSelectedFare :: Money,
    actualRideDistance :: HighPrecMeters,
    rideRating :: Maybe Int,
    riderName :: Maybe Text,
    tripStartTime :: Maybe UTCTime,
    tripEndTime :: Maybe UTCTime,
    specialLocationTag :: Maybe Text,
    chargeableDistance :: Maybe Meters,
    exoPhone :: Text,
    bapName :: Maybe Text,
    bapLogo :: Maybe BaseUrl,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    customerExtraFee :: Maybe Money,
    disabilityTag :: Maybe Text,
    requestedVehicleVariant :: DVeh.Variant,
    driverGoHomeRequestId :: Maybe (Id DDGR.DriverGoHomeRequest)
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data OTPRideReq = OTPRideReq
  { specialZoneOtpCode :: Text,
    point :: LatLong
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

newtype DriverRideListRes = DriverRideListRes
  { list :: [DriverRideRes]
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

listDriverRides ::
  (EsqDBReplicaFlow m r, EncFlow m r, EsqDBFlow m r, CacheFlow m r) =>
  Id DP.Person ->
  Maybe Integer ->
  Maybe Integer ->
  Maybe Bool ->
  Maybe DRide.RideStatus ->
  Maybe Day ->
  m DriverRideListRes
listDriverRides driverId mbLimit mbOffset mbOnlyActive mbRideStatus mbDay = do
  rides <- runInReplica $ QRide.findAllByDriverId driverId mbLimit mbOffset mbOnlyActive mbRideStatus mbDay
  driverRideLis <- forM rides $ \(ride, booking) -> do
    rideDetail <- runInReplica $ QRD.findById ride.id >>= fromMaybeM (VehicleNotFound driverId.getId)
    rideRating <- runInReplica $ QR.findRatingForRide ride.id
    driverNumber <- RD.getDriverNumber rideDetail
    mbExophone <- CQExophone.findByPrimaryPhone booking.primaryExophone
    bapMetadata <- CQSM.findById (Id booking.bapId)
    let goHomeReqId = ride.driverGoHomeRequestId
    pure $ mkDriverRideRes rideDetail driverNumber rideRating mbExophone (ride, booking) bapMetadata goHomeReqId
  pure . DriverRideListRes $ driverRideLis

mkDriverRideRes ::
  RD.RideDetails ->
  Maybe Text ->
  Maybe DRating.Rating ->
  Maybe DExophone.Exophone ->
  (DRide.Ride, DRB.Booking) ->
  Maybe DSM.BapMetadata ->
  Maybe (Id DDGR.DriverGoHomeRequest) ->
  DriverRideRes
mkDriverRideRes rideDetails driverNumber rideRating mbExophone (ride, booking) bapMetadata goHomeReqId = do
  let fareParams = booking.fareParams
      estimatedBaseFare =
        fareSum $
          fareParams{driverSelectedFare = Nothing -- it should not be part of estimatedBaseFare
                    }
  let initial = "" :: Text
  DriverRideRes
    { id = ride.id,
      shortRideId = ride.shortId,
      status = ride.status,
      fromLocation = DBLoc.makeBookingLocationAPIEntity booking.fromLocation,
      toLocation = DBLoc.makeBookingLocationAPIEntity booking.toLocation,
      driverName = rideDetails.driverName,
      driverNumber,
      vehicleNumber = rideDetails.vehicleNumber,
      vehicleColor = fromMaybe initial rideDetails.vehicleColor,
      vehicleVariant = fromMaybe DVeh.SEDAN rideDetails.vehicleVariant,
      vehicleModel = fromMaybe initial rideDetails.vehicleModel,
      computedFare = ride.fare,
      estimatedBaseFare = estimatedBaseFare,
      estimatedDistance = booking.estimatedDistance,
      driverSelectedFare = fromMaybe 0 fareParams.driverSelectedFare,
      actualRideDistance = ride.traveledDistance,
      createdAt = ride.createdAt,
      updatedAt = ride.updatedAt,
      riderName = booking.riderName,
      pickupDropOutsideOfThreshold = ride.pickupDropOutsideOfThreshold,
      tripStartTime = ride.tripStartTime,
      tripEndTime = ride.tripEndTime,
      specialLocationTag = booking.specialLocationTag,
      rideRating = rideRating <&> (.ratingValue),
      chargeableDistance = ride.chargeableDistance,
      exoPhone = maybe booking.primaryExophone (\exophone -> if not exophone.isPrimaryDown then exophone.primaryPhone else exophone.backupPhone) mbExophone,
      customerExtraFee = fareParams.customerExtraFee,
      bapName = bapMetadata <&> (.name),
      bapLogo = bapMetadata <&> (.logoUrl),
      disabilityTag = booking.disabilityTag,
      requestedVehicleVariant = booking.vehicleVariant,
      driverGoHomeRequestId = goHomeReqId
    }

arrivedAtPickup :: (EncFlow m r, CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, HasShortDurationRetryCfg r c, HasFlowEnv m r '["nwAddress" ::: BaseUrl], HasHttpClientOptions r c, HasFlowEnv m r '["driverReachedDistance" ::: HighPrecMeters]) => Id DRide.Ride -> LatLong -> m APISuccess
arrivedAtPickup rideId req = do
  ride <- runInReplica (QRide.findById rideId) >>= fromMaybeM (RideDoesNotExist rideId.getId)
  unless (isValidRideStatus (ride.status)) $ throwError $ RideInvalidStatus "The ride has already started."
  booking <- runInReplica $ QBooking.findById ride.bookingId >>= fromMaybeM (BookingDoesNotExist ride.bookingId.getId)
  let pickupLoc = getCoordinates booking.fromLocation
  let distance = distanceBetweenInMeters req pickupLoc
  driverReachedDistance <- asks (.driverReachedDistance)
  unless (distance < driverReachedDistance) $ throwError $ DriverNotAtPickupLocation ride.driverId.getId
  unless (isJust ride.driverArrivalTime) $ do
    _ <- QRide.updateArrival rideId
    _ <- QDFS.updateStatus ride.driverId DDFS.WAITING_FOR_CUSTOMER {rideId}
    BP.sendDriverArrivalUpdateToBAP booking ride ride.driverArrivalTime
  pure Success
  where
    isValidRideStatus status = status == DRide.NEW

otpRideCreate :: DP.Person -> Text -> DRB.Booking -> Flow DriverRideRes
otpRideCreate driver otpCode booking = do
  transporter <-
    QM.findById booking.providerId
      >>= fromMaybeM (MerchantNotFound booking.providerId.getId)
  vehicle <- QVeh.findById driver.id >>= fromMaybeM (VehicleNotFound driver.id.getId)
  when (isNotAllowedVehicleVariant vehicle.variant booking.vehicleVariant) $ throwError $ InvalidRequest "Wrong Vehicle Variant"
  when (booking.status `elem` [DRB.COMPLETED, DRB.CANCELLED]) $ throwError (BookingInvalidStatus $ show booking.status)

  driverInfo <- QDI.findById (cast driver.id) >>= fromMaybeM DriverInfoNotFound
  unless (driverInfo.subscribed) $ throwError DriverUnsubscribed
  unless (driverInfo.enabled) $ throwError DriverAccountDisabled
  when driverInfo.onRide $ throwError DriverOnRide
  ghrId <- (CGHR.getDriverGoHomeRequestInfo driver.id booking.providerId) Nothing <&> (.driverGoHomeRequestId)
  ride <- buildRide otpCode driver.id (Just transporter.id) ghrId
  rideDetails <- buildRideDetails ride

  -- moving route from booking id to ride id
  routeInfo :: Maybe RI.RouteInfo <- Redis.safeGet (BS.searchRequestKey $ getId booking.id)
  case routeInfo of
    Just route -> Redis.setExp (BS.searchRequestKey $ getId ride.id) route 14400
    Nothing -> logDebug "Unable to get the key"

  enableLocationTrackingService <- asks (.enableLocationTrackingService)
  when enableLocationTrackingService $
    void $ LF.rideDetails ride.id ride.status transporter.id ride.driverId booking.fromLocation.lat booking.fromLocation.lon
  QBooking.updateStatus booking.id DRB.TRIP_ASSIGNED
  QRide.create ride
  DLoc.updateOnRide driver.merchantId driver.id True

  QDFS.updateStatus driver.id DDFS.RIDE_ASSIGNED {rideId = ride.id}
  QRideD.create rideDetails

  QBE.logDriverAssignedEvent (cast driver.id) booking.id ride.id
  triggerRideCreatedEvent RideEventData {ride = ride, personId = driver.id, merchantId = transporter.id}

  uBooking <- runInReplica $ QBooking.findById booking.id >>= fromMaybeM (BookingNotFound booking.id.getId) -- in replica db we can have outdated value
  Notify.notifyDriver transporter.id notificationType notificationTitle (message uBooking) driver.id driver.deviceToken

  handle (errHandler uBooking transporter) $ BP.sendRideAssignedUpdateToBAP uBooking ride

  DS.driverScoreEventHandler DST.OnNewRideAssigned {merchantId = transporter.id, driverId = driver.id}

  driverNumber <- RD.getDriverNumber rideDetails
  mbExophone <- CQExophone.findByPrimaryPhone booking.primaryExophone
  bapMetadata <- CQSM.findById (Id booking.bapId)
  pure $ mkDriverRideRes rideDetails driverNumber Nothing mbExophone (ride, booking) bapMetadata ride.driverGoHomeRequestId
  where
    errHandler uBooking transporter exc
      | Just BecknAPICallError {} <- fromException @BecknAPICallError exc = DConfirm.cancelBooking uBooking (Just driver) transporter >> throwM exc
      | Just ExternalAPICallError {} <- fromException @ExternalAPICallError exc = DConfirm.cancelBooking uBooking (Just driver) transporter >> throwM exc
      | otherwise = throwM exc

    notificationType = FCM.DRIVER_ASSIGNMENT
    notificationTitle = "Driver has been assigned the ride!"
    message uBooking =
      cs $
        unwords
          [ "You have been assigned a ride for",
            cs (showTimeIst uBooking.startTime) <> ".",
            "Check the app for more details."
          ]
    buildRide otp driverId merchantId ghrId = do
      guid <- Id <$> generateGUID
      shortId <- generateShortId
      now <- getCurrentTime
      trackingUrl <- buildTrackingUrl guid
      return
        DRide.Ride
          { id = guid,
            pickupDropOutsideOfThreshold = Nothing,
            bookingId = booking.id,
            shortId = shortId,
            merchantId = merchantId,
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
            fareParametersId = Nothing,
            distanceCalculationFailed = Nothing,
            createdAt = now,
            updatedAt = now,
            driverDeviatedFromRoute = Just False,
            numberOfSnapToRoadCalls = Nothing,
            numberOfDeviation = Nothing,
            driverGoHomeRequestId = ghrId
          }

    buildTrackingUrl rideId = do
      (bppUIUrl :: BaseUrl) <- asks (.selfUIUrl)
      let rideid = T.unpack (getId rideId)
      return $
        bppUIUrl
          { baseUrlPath = baseUrlPath bppUIUrl <> "/driver/location/" <> rideid
          }

    buildRideDetails ride = do
      vehicle <-
        QVeh.findById ride.driverId
          >>= fromMaybeM (VehicleNotFound ride.driverId.getId)
      return
        DRD.RideDetails
          { id = ride.id,
            driverName = driver.firstName,
            driverNumber = driver.mobileNumber,
            driverCountryCode = driver.mobileCountryCode,
            vehicleNumber = vehicle.registrationNo,
            vehicleColor = Just vehicle.color,
            vehicleVariant = Just vehicle.variant,
            vehicleModel = Just vehicle.model,
            vehicleClass = Nothing
          }
    isNotAllowedVehicleVariant driverVehicle bookingVehicle =
      (bookingVehicle == DVeh.TAXI_PLUS || bookingVehicle == DVeh.SEDAN || bookingVehicle == DVeh.SUV || bookingVehicle == DVeh.HATCHBACK)
        && driverVehicle == DVeh.TAXI
