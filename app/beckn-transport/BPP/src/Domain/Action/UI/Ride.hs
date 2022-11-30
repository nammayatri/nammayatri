module Domain.Action.UI.Ride
  ( DriverRideRes (..),
    DriverRideListRes (..),
    listDriverRides,
  )
where

import Beckn.Prelude
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.Booking.BookingLocation as DBLoc
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.RideDetails as RD
import Domain.Types.Vehicle as VD
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RideDetails as QRD
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
  (EsqDBFlow m r, EncFlow m r) =>
  Id DP.Person ->
  Maybe Integer ->
  Maybe Integer ->
  Maybe Bool ->
  m DriverRideListRes
listDriverRides driverId mbLimit mbOffset mbOnlyActive = do
  rides <- QRide.findAllByDriverId driverId mbLimit mbOffset mbOnlyActive
  driverRideLis <- forM rides $ \(ride, booking) -> do
    rideDetail <- QRD.findById ride.id >>= fromMaybeM (VehicleNotFound driverId.getId)
    driverNumber <- RD.getDriverNumber rideDetail
    pure $ mkDriverRideRes rideDetail driverNumber (ride, booking)
  pure . DriverRideListRes $ driverRideLis

mkDriverRideRes ::
  RD.RideDetails ->
  Maybe Text ->
  (DRide.Ride, DRB.Booking) ->
  DriverRideRes
mkDriverRideRes rideDetails  driverNumber (ride, booking) = do
  let mbToLocation = case booking.bookingDetails of
        DRB.OneWayDetails details -> Just details.toLocation
        DRB.RentalDetails _ -> Nothing
  let initial = "" :: Text
  DriverRideRes
    { id = ride.id,
      shortRideId = ride.shortId,
      status = ride.status,
      fromLocation = DBLoc.makeBookingLocationAPIEntity booking.fromLocation,
      toLocation = DBLoc.makeBookingLocationAPIEntity <$> mbToLocation,
      estimatedFare = booking.estimatedFare,
      estimatedTotalFare = booking.estimatedTotalFare,
      discount = booking.discount,
      driverName = rideDetails.driverName,
      driverNumber,
      vehicleNumber = rideDetails.vehicleNumber,
      vehicleColor = fromMaybe initial rideDetails.vehicleColor,
      vehicleVariant = fromMaybe SEDAN rideDetails.vehicleVariant,
      vehicleModel = fromMaybe initial rideDetails.vehicleModel,
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
