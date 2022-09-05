module Domain.Action.UI.Ride
  ( DriverRideRes (..),
    DriverRideListRes (..),
    listDriverRides,
  )
where

import Beckn.Prelude
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.Booking.BookingLocation as DBLoc
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.Vehicle as DVeh
import SharedLogic.FareCalculator (baseFareSumRounded)
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.Vehicle as QVeh
import Types.Error

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
    driverSelectedFare :: Money,
    actualRideDistance :: HighPrecMeters,
    rideRating :: Maybe Int,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    riderName :: Maybe Text,
    tripStartTime :: Maybe UTCTime,
    tripEndTime :: Maybe UTCTime
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

newtype DriverRideListRes = DriverRideListRes
  { list :: [DriverRideRes]
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

listDriverRides ::
  (EsqDBFlow m r, EncFlow m r) =>
  Id DP.Person ->
  Maybe Integer ->
  Maybe Integer ->
  Maybe Bool ->
  m DriverRideListRes
listDriverRides driverId mbLimit mbOffset mbOnlyActive = do
  rideData <- QRide.findAllByDriverId driverId mbLimit mbOffset mbOnlyActive
  vehicle <- QVeh.findById driverId >>= fromMaybeM (VehicleNotFound driverId.getId)
  driver <- QP.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  driverNumber <- DP.getPersonNumber driver
  pure . DriverRideListRes $ mkDriverRideRes vehicle driver driverNumber <$> rideData

mkDriverRideRes ::
  DVeh.Vehicle ->
  DP.Person ->
  Maybe Text ->
  (DRide.Ride, DRB.Booking) ->
  DriverRideRes
mkDriverRideRes vehicle driver driverNumber (ride, booking) = do
  let fareParams = booking.fareParams
  DriverRideRes
    { id = ride.id,
      shortRideId = ride.shortId,
      status = ride.status,
      fromLocation = DBLoc.makeBookingLocationAPIEntity booking.fromLocation,
      toLocation = DBLoc.makeBookingLocationAPIEntity booking.toLocation,
      driverName = driver.firstName,
      driverNumber = driverNumber,
      vehicleNumber = vehicle.registrationNo,
      vehicleColor = vehicle.color,
      vehicleVariant = vehicle.variant,
      vehicleModel = vehicle.model,
      computedFare = ride.fare,
      estimatedBaseFare = baseFareSumRounded fareParams,
      driverSelectedFare = fromMaybe 0 fareParams.driverSelectedFare,
      actualRideDistance = ride.traveledDistance,
      createdAt = ride.createdAt,
      updatedAt = ride.updatedAt,
      riderName = booking.riderName,
      tripStartTime = ride.tripStartTime,
      tripEndTime = ride.tripEndTime,
      rideRating = ride.rideRating <&> (.ratingValue)
    }
