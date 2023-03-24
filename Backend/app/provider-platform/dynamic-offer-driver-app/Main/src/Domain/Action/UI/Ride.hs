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
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.Booking.BookingLocation as DBLoc
import qualified Domain.Types.Driver.DriverFlowStatus as DDFS
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Rating as DRating
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.RideDetails as DRD
import qualified Domain.Types.RideDetails as RD
import qualified Domain.Types.Vehicle as DVeh
import qualified Kernel.External.FCM.Types as FCM
import Kernel.External.Maps (HasCoordinates (getCoordinates))
import Kernel.External.Maps.Types
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Storage.Esqueleto.Transactionable (runInReplica)
import Kernel.Storage.Hedis as Redis (HedisFlow)
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.APISuccess
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Kernel.Utils.Common
import Servant.Client (BaseUrl (..))
import qualified SharedLogic.CallBAP as BP
import qualified SharedLogic.DriverLocation as DLoc
import SharedLogic.DriverPool
import SharedLogic.FareCalculator
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.DriverInformation as QDriverInformation
import Storage.CachedQueries.Merchant as QM
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.BusinessEvent as QBE
import qualified Storage.Queries.Driver.DriverFlowStatus as QDFS
import qualified Storage.Queries.Rating as QR
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RideDetails as QRD
import qualified Storage.Queries.RideDetails as QRideD
import Storage.Queries.Vehicle as QVeh
import Tools.Error
import qualified Lib.DriverScore as DS  
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
    vehicleModel :: Text,
    vehicleColor :: Text,
    vehicleNumber :: Text,
    computedFare :: Maybe Money,
    estimatedBaseFare :: Money,
    estimatedDistance :: Meters,
    driverSelectedFare :: Money,
    actualRideDistance :: HighPrecMeters,
    rideRating :: Maybe Int,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    riderName :: Maybe Text,
    tripStartTime :: Maybe UTCTime,
    tripEndTime :: Maybe UTCTime,
    chargeableDistance :: Maybe Meters,
    exoPhone :: Text
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
  (EsqDBReplicaFlow m r, EncFlow m r) =>
  Id DP.Person ->
  Maybe Integer ->
  Maybe Integer ->
  Maybe Bool ->
  Maybe DRide.RideStatus ->
  m DriverRideListRes
listDriverRides driverId mbLimit mbOffset mbOnlyActive mbRideStatus = do
  rides <- runInReplica $ QRide.findAllByDriverId driverId mbLimit mbOffset mbOnlyActive mbRideStatus
  driverRideLis <- forM rides $ \(ride, booking) -> do
    rideDetail <- runInReplica $ QRD.findById ride.id >>= fromMaybeM (VehicleNotFound driverId.getId)
    rideRating <- runInReplica $ QR.findRatingForRide ride.id
    driverNumber <- RD.getDriverNumber rideDetail
    pure $ mkDriverRideRes rideDetail driverNumber rideRating (ride, booking)
  pure . DriverRideListRes $ driverRideLis

mkDriverRideRes ::
  RD.RideDetails ->
  Maybe Text ->
  Maybe DRating.Rating ->
  (DRide.Ride, DRB.Booking) ->
  DriverRideRes
mkDriverRideRes rideDetails driverNumber rideRating (ride, booking) = do
  let fareParams = booking.fareParams
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
      estimatedBaseFare = baseFareSum fareParams + (fromMaybe 0 fareParams.deadKmFare),
      estimatedDistance = booking.estimatedDistance,
      driverSelectedFare = fromMaybe 0 fareParams.driverSelectedFare,
      actualRideDistance = ride.traveledDistance,
      createdAt = ride.createdAt,
      updatedAt = ride.updatedAt,
      riderName = booking.riderName,
      tripStartTime = ride.tripStartTime,
      tripEndTime = ride.tripEndTime,
      rideRating = rideRating <&> (.ratingValue),
      chargeableDistance = ride.chargeableDistance,
      exoPhone = booking.providerExoPhone
    }

arrivedAtPickup :: (EncFlow m r, CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, CoreMetrics m, HasShortDurationRetryCfg r c, HasFlowEnv m r '["nwAddress" ::: BaseUrl], HasHttpClientOptions r c, HasFlowEnv m r '["driverReachedDistance" ::: HighPrecMeters]) => Id DRide.Ride -> LatLong -> m APISuccess
arrivedAtPickup rideId req = do
  ride <- runInReplica $ QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  unless (isValidRideStatus (ride.status)) $ throwError $ RideInvalidStatus "The ride has already started."
  booking <- runInReplica $ QBooking.findById ride.bookingId >>= fromMaybeM (BookingDoesNotExist ride.bookingId.getId)
  let pickupLoc = getCoordinates booking.fromLocation
  let distance = distanceBetweenInMeters req pickupLoc
  driverReachedDistance <- asks (.driverReachedDistance)
  unless (distance < driverReachedDistance) $ throwError $ DriverNotAtPickupLocation ride.driverId.getId
  unless (isJust ride.driverArrivalTime) $ do
    Esq.runTransaction $ do
      QRide.updateArrival rideId
      QDFS.updateStatus ride.driverId DDFS.WAITING_FOR_CUSTOMER {rideId}
    BP.sendDriverArrivalUpdateToBAP booking ride ride.driverArrivalTime
  pure Success
  where
    isValidRideStatus status = status == DRide.NEW

otpRideCreate ::
  ( HasCacheConfig r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    Redis.HedisFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasFlowEnv m r '["selfUIUrl" ::: BaseUrl],
    HasDriverPoolConfig r,
    HasHttpClientOptions r c,
    EncFlow m r,
    HasShortDurationRetryCfg r c,
    CoreMetrics m,
    HasPrettyLogger m r
  ) =>
  DP.Person ->
  OTPRideReq ->
  m DriverRideRes
otpRideCreate driver req = do
  now <- getCurrentTime
  booking <- runInReplica $ QBooking.findBookingBySpecialZoneOTP driver.merchantId req.specialZoneOtpCode now >>= fromMaybeM (BookingNotFoundForSpecialZoneOtp req.specialZoneOtpCode)
  transporter <-
    QM.findById booking.providerId
      >>= fromMaybeM (MerchantNotFound booking.providerId.getId)

  driverInfo <- QDriverInformation.findById (cast driver.id) >>= fromMaybeM DriverInfoNotFound
  when driverInfo.onRide $ throwError DriverOnRide
  let otpCode = req.specialZoneOtpCode
  ride <- buildRide booking otpCode driver.id
  rideDetails <- buildRideDetails ride
  Esq.runTransaction $ do
    QBooking.updateStatus booking.id DRB.TRIP_ASSIGNED
    QRide.create ride
    QDFS.updateStatus driver.id DDFS.RIDE_ASSIGNED {rideId = ride.id}
    QRideD.create rideDetails
    QBE.logDriverAssignedEvent (cast driver.id) booking.id ride.id
  DLoc.updateOnRide (cast driver.id) True
  uBooking <- runInReplica $ QBooking.findById booking.id >>= fromMaybeM (BookingNotFound booking.id.getId)
  Notify.notifyDriver transporter.id notificationType notificationTitle (message uBooking) driver.id driver.deviceToken
  void $ BP.sendRideAssignedUpdateToBAP uBooking ride
  DS.driverScoreEventHandler (DS.OnNewRideAssigned transporter.id driver.id)
  driverNumber <- RD.getDriverNumber rideDetails
  pure $ mkDriverRideRes rideDetails driverNumber Nothing (ride, booking)
  where
    notificationType = FCM.DRIVER_ASSIGNMENT
    notificationTitle = "Driver has been assigned the ride!"
    message booking =
      cs $
        unwords
          [ "You have been assigned a ride for",
            cs (showTimeIst booking.startTime) <> ".",
            "Check the app for more details."
          ]
    buildRide booking otp driverId = do
      guid <- Id <$> generateGUID
      shortId <- generateShortId
      now <- getCurrentTime
      trackingUrl <- buildTrackingUrl guid
      return
        DRide.Ride
          { id = guid,
            bookingId = booking.id,
            shortId = shortId,
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
            createdAt = now,
            updatedAt = now
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
