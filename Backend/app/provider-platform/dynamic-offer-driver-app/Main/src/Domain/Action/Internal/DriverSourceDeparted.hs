module Domain.Action.Internal.DriverSourceDeparted where

import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import Domain.Types.TripTransaction
import Domain.Types.VehicleCategory
import Domain.Types.VehicleVariant
import Environment
import Kernel.External.Maps.Types
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.WMB as WMB
import qualified Storage.Queries.Route as QR
import qualified Storage.Queries.TripTransaction as QTT
import Tools.Error

data DriverSourceDepartedReq = DriverSourceDepartedReq
  { rideId :: Id DRide.Ride,
    driverId :: Id DP.Person,
    location :: LatLong,
    vehicleVariant :: VehicleVariant
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

driverSourceDeparted :: DriverSourceDepartedReq -> Flow APISuccess
driverSourceDeparted req = do
  let vehicleCategory = castVehicleVariantToVehicleCategory req.vehicleVariant
  case vehicleCategory of
    BUS -> do
      let tripTransactionId = cast @DRide.Ride @TripTransaction req.rideId
      tripTransaction <- QTT.findByTransactionId tripTransactionId >>= fromMaybeM (TripTransactionNotFound tripTransactionId.getId)
      unless (tripTransaction.status == TRIP_ASSIGNED) $ throwError AlreadyOnActiveTrip
      closestStop <- WMB.findClosestStop tripTransaction.routeCode req.location >>= fromMaybeM (StopNotFound)
      route <- QR.findByRouteCode tripTransaction.routeCode >>= fromMaybeM (RouteNotFound tripTransaction.routeCode)
      (sourceStopInfo, destinationStopInfo) <- WMB.getSourceAndDestinationStopInfo route tripTransaction.routeCode
      void $ WMB.startTripTransaction tripTransaction (Just route) (Just closestStop) sourceStopInfo req.location destinationStopInfo.point True AutoDetect
    category -> throwError $ InvalidRequest ("Unsupported vehicle category, " <> show category)
  pure Success
