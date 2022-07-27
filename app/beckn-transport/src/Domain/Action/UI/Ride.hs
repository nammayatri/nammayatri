module Domain.Action.UI.Ride
  ( DriverRideRes (..),
    DriverRideListRes (..),
    listDriverRides,
  )
where

import Beckn.Prelude
import Beckn.Types.Amount
import Beckn.Types.Id
import qualified Domain.Types.BookingLocation as DBLoc
import qualified Domain.Types.Person as SP
import qualified Domain.Types.Ride as SRide
import qualified Domain.Types.RideBooking as SRB
import Domain.Types.Vehicle (Variant)
import qualified Storage.Queries.BookingLocation as QBLoc
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.Vehicle as QVeh
import Types.Error
import Utils.Common

data DriverRideRes = DriverRideRes
  { id :: Id SRide.Ride,
    shortRideId :: ShortId SRide.Ride,
    status :: SRide.RideStatus,
    fromLocation :: DBLoc.BookingLocationAPIEntity,
    toLocation :: Maybe DBLoc.BookingLocationAPIEntity,
    discount :: Maybe Amount,
    driverName :: Text,
    driverNumber :: Maybe Text,
    vehicleVariant :: Variant,
    vehicleModel :: Text,
    vehicleColor :: Text,
    vehicleNumber :: Text,
    estimatedFare :: Amount,
    estimatedTotalFare :: Amount,
    computedFare :: Maybe Amount,
    computedTotalFare :: Maybe Amount,
    actualRideDistance :: HighPrecMeters,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
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
  Id SP.Person ->
  Maybe Integer ->
  Maybe Integer ->
  Maybe Bool ->
  m DriverRideListRes
listDriverRides driverId mbLimit mbOffset mbOnlyActive = do
  rideData <- QRide.findAllByDriverId driverId mbLimit mbOffset mbOnlyActive
  DriverRideListRes <$> traverse buildDriverRideRes rideData

buildDriverRideRes :: (EsqDBFlow m r, EncFlow m r) => (SRide.Ride, SRB.RideBooking) -> m DriverRideRes
buildDriverRideRes (ride, rideBooking) = do
  fromLocation <- QBLoc.findById rideBooking.fromLocationId >>= fromMaybeM LocationNotFound
  toLocation <- case rideBooking.rideBookingDetails of
    SRB.OneWayDetails details -> QBLoc.findById details.toLocationId >>= fromMaybeM LocationNotFound . Just
    SRB.RentalDetails _ -> pure Nothing

  vehicle <- QVeh.findById ride.driverId >>= fromMaybeM (VehicleNotFound ride.driverId.getId)
  driver <- QPerson.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
  driverNumber <- SP.getPersonNumber driver
  pure
    DriverRideRes
      { id = ride.id,
        shortRideId = ride.shortId,
        status = ride.status,
        fromLocation = DBLoc.makeBookingLocationAPIEntity fromLocation,
        toLocation = DBLoc.makeBookingLocationAPIEntity <$> toLocation,
        estimatedFare = rideBooking.estimatedFare,
        estimatedTotalFare = rideBooking.estimatedTotalFare,
        discount = rideBooking.discount,
        driverName = driver.firstName,
        driverNumber = driverNumber,
        vehicleNumber = vehicle.registrationNo,
        vehicleColor = vehicle.color,
        vehicleVariant = vehicle.variant,
        vehicleModel = vehicle.model,
        computedFare = ride.fare,
        computedTotalFare = ride.totalFare,
        actualRideDistance = ride.traveledDistance,
        createdAt = ride.createdAt,
        updatedAt = ride.updatedAt,
        tripStartTime = ride.tripStartTime,
        tripEndTime = ride.tripEndTime
      }
