module Product.Ride where

import Beckn.Prelude
import Beckn.Types.Id
import qualified Domain.Types.Person as SP
import qualified Domain.Types.Ride as SRide
import qualified Domain.Types.RideBooking as SRB
import qualified Domain.Types.RideBooking.BookingLocation as DBLoc
import Environment
import Product.FareCalculator.Calculator
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.Vehicle as QVeh
import qualified Types.API.Ride as API
import Types.Error
import Utils.Common

listDriverRides ::
  Id SP.Person ->
  Maybe Integer ->
  Maybe Integer ->
  Maybe Bool ->
  FlowHandler API.DriverRideListRes
listDriverRides driverId mbLimit mbOffset mbOnlyActive = withFlowHandlerAPI $ do
  rideData <- QRide.findAllByDriverId driverId mbLimit mbOffset mbOnlyActive
  API.DriverRideListRes <$> traverse buildDriverRideRes rideData

buildDriverRideRes :: (SRide.Ride, SRB.RideBooking) -> Flow API.DriverRideRes
buildDriverRideRes (ride, rideBooking) = do
  vehicle <- QVeh.findById ride.driverId >>= fromMaybeM (VehicleNotFound ride.driverId.getId)
  driver <- QP.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
  driverNumber <- SP.getPersonNumber driver
  let fareParams = rideBooking.fareParams
  pure
    API.DriverRideRes
      { id = ride.id,
        shortRideId = ride.shortId,
        status = ride.status,
        fromLocation = DBLoc.makeBookingLocationAPIEntity rideBooking.fromLocation,
        toLocation = DBLoc.makeBookingLocationAPIEntity rideBooking.toLocation,
        estimatedBaseFare = baseFareSum fareParams,
        driverSelectedFare = fromMaybe 0 fareParams.driverSelectedFare,
        driverName = driver.firstName,
        driverNumber = driverNumber,
        vehicleNumber = vehicle.registrationNo,
        vehicleColor = vehicle.color,
        vehicleVariant = vehicle.variant,
        vehicleModel = vehicle.model,
        actualRideDistance = ride.traveledDistance,
        createdAt = ride.createdAt,
        updatedAt = ride.updatedAt
      }
