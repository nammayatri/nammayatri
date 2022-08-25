module Product.Ride where

import Beckn.Prelude
import Beckn.Types.Id
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.Booking.BookingLocation as DBLoc
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.Vehicle as DVeh
import Environment
import Product.FareCalculator.Calculator (baseFareSumRounded)
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.Vehicle as QVeh
import qualified Types.API.Ride as API
import Types.Error
import Utils.Common

listDriverRides ::
  Id DP.Person ->
  Maybe Integer ->
  Maybe Integer ->
  Maybe Bool ->
  FlowHandler API.DriverRideListRes
listDriverRides driverId mbLimit mbOffset mbOnlyActive = withFlowHandlerAPI $ do
  rideData <- QRide.findAllByDriverId driverId mbLimit mbOffset mbOnlyActive
  vehicle <- QVeh.findById driverId >>= fromMaybeM (VehicleNotFound driverId.getId)
  driver <- QP.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  driverNumber <- DP.getPersonNumber driver
  pure . API.DriverRideListRes $ mkDriverRideRes vehicle driver driverNumber <$> rideData

mkDriverRideRes ::
  DVeh.Vehicle ->
  DP.Person ->
  Maybe Text ->
  (DRide.Ride, DRB.Booking) ->
  API.DriverRideRes
mkDriverRideRes vehicle driver driverNumber (ride, booking) = do
  let fareParams = booking.fareParams
  API.DriverRideRes
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
