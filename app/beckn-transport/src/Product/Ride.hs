module Product.Ride where

import App.Types
import Beckn.Prelude
import Beckn.Types.Id
import qualified Domain.Types.Person as SP
import qualified Domain.Types.Ride as SRide
import qualified Domain.Types.RideBooking as SRB
import qualified Domain.Types.SearchReqLocation as SLoc
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.SearchReqLocation as QLoc
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
  fromLocation <- QLoc.findById rideBooking.fromLocationId >>= fromMaybeM LocationNotFound
  toLocation <- QLoc.findById rideBooking.toLocationId >>= fromMaybeM LocationNotFound
  vehicle <- QVeh.findById ride.vehicleId >>= fromMaybeM VehicleNotFound
  driver <- QP.findById ride.driverId >>= fromMaybeM PersonNotFound
  driverNumber <- SP.getPersonNumber driver
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
        driverNumber = driverNumber,
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
