module Product.Ride where

import App.Types
import Beckn.External.Encryption (decrypt)
import Beckn.Prelude
import Beckn.Types.Id
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.SearchReqLocation as QLoc
import qualified Storage.Queries.Vehicle as QVeh
import qualified Types.API.Ride as API
import Types.Error
import qualified Types.Storage.Person as SP
import qualified Types.Storage.Ride as SRide
import qualified Types.Storage.RideBooking as SRB
import qualified Types.Storage.SearchReqLocation as SLoc
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
  fromLocation <- QLoc.findLocationById rideBooking.fromLocationId >>= fromMaybeM LocationNotFound
  toLocation <- QLoc.findLocationById rideBooking.toLocationId >>= fromMaybeM LocationNotFound
  vehicle <- QVeh.findVehicleById ride.vehicleId >>= fromMaybeM VehicleNotFound
  driver <- QP.findPersonById ride.driverId >>= fromMaybeM PersonNotFound
  decDriver <- decrypt driver
  pure
    API.DriverRideRes
      { id = ride.id,
        shortRideId = ride.shortId,
        status = ride.status,
        fromLocation = SLoc.makeSearchReqLocationAPIEntity fromLocation,
        toLocation = SLoc.makeSearchReqLocationAPIEntity toLocation,
        estimatedFare = rideBooking.estimatedFare,
        estimatedTotalFare = rideBooking.estimatedTotalFare,
        discount = rideBooking.discount,
        driverName = driver.firstName,
        driverNumber = decDriver.mobileCountryCode <> decDriver.mobileNumber,
        vehicleNumber = vehicle.registrationNo,
        vehicleColor = vehicle.color,
        vehicleVariant = vehicle.variant,
        vehicleModel = vehicle.model,
        computedFare = ride.fare,
        computedTotalFare = ride.totalFare,
        actualRideDistance = ride.traveledDistance,
        createdAt = ride.createdAt,
        updatedAt = ride.updatedAt
      }
