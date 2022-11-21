module Domain.Action.UI.Ride
  ( DriverRideRes (..),
    DriverRideListRes (..),
    listDriverRides,
  )
where

import Beckn.Prelude
import Beckn.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.Booking.BookingLocation as DBLoc
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import Domain.Types.Vehicle (Variant)
import qualified Domain.Types.Vehicle as DVeh
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.Vehicle as QVeh
import Tools.Error

data DriverRideRes = DriverRideRes
  { id :: Id DRide.Ride,
    shortRideId :: ShortId DRide.Ride,
    status :: DRide.RideStatus,
    fromLocation :: DBLoc.BookingLocationAPIEntity,
    toLocation :: Maybe DBLoc.BookingLocationAPIEntity,
    discount :: Maybe Money,
    driverName :: Text,
    driverNumber :: Maybe Text,
    vehicleVariant :: Variant,
    vehicleModel :: Text,
    vehicleColor :: Text,
    vehicleNumber :: Text,
    estimatedFare :: Money,
    estimatedTotalFare :: Money,
    computedFare :: Maybe Money,
    computedTotalFare :: Maybe Money,
    actualRideDistance :: Meters,
    rideRating :: Maybe Int,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    tripStartTime :: Maybe UTCTime,
    tripEndTime :: Maybe UTCTime,
    chargeableDistance :: Maybe Meters
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
  m DriverRideListRes
listDriverRides driverId mbLimit mbOffset mbOnlyActive = do
  rideData <- QRide.findAllByDriverId driverId mbLimit mbOffset mbOnlyActive
  vehicle <- QVeh.findById driverId >>= fromMaybeM (VehicleNotFound driverId.getId)
  driver <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  driverNumber <- DP.getPersonNumber driver
  pure . DriverRideListRes $ mkDriverRideRes vehicle driver driverNumber <$> rideData

mkDriverRideRes ::
  DVeh.Vehicle ->
  DP.Person ->
  Maybe Text ->
  (DRide.Ride, DRB.Booking) ->
  DriverRideRes
mkDriverRideRes vehicle driver driverNumber (ride, booking) = do
  let mbToLocation = case booking.bookingDetails of
        DRB.OneWayDetails details -> Just details.toLocation
        DRB.RentalDetails _ -> Nothing
  DriverRideRes
    { id = ride.id,
      shortRideId = ride.shortId,
      status = ride.status,
      fromLocation = DBLoc.makeBookingLocationAPIEntity booking.fromLocation,
      toLocation = DBLoc.makeBookingLocationAPIEntity <$> mbToLocation,
      estimatedFare = booking.estimatedFare,
      estimatedTotalFare = booking.estimatedTotalFare,
      discount = booking.discount,
      driverName = driver.firstName,
      driverNumber = driverNumber,
      vehicleNumber = vehicle.registrationNo,
      vehicleColor = vehicle.color,
      vehicleVariant = vehicle.variant,
      vehicleModel = vehicle.model,
      computedFare = ride.fare,
      computedTotalFare = ride.totalFare,
      actualRideDistance = roundToIntegral ride.traveledDistance,
      rideRating = ride.rideRating <&> (.ratingValue),
      createdAt = ride.createdAt,
      updatedAt = ride.updatedAt,
      tripStartTime = ride.tripStartTime,
      tripEndTime = ride.tripEndTime,
      chargeableDistance = ride.chargeableDistance
    }
