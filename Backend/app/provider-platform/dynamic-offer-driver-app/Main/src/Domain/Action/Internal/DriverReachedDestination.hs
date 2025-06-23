module Domain.Action.Internal.DriverReachedDestination where

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
import qualified Storage.Queries.FleetConfig as QFC
import qualified Storage.Queries.TripTransaction as QTT
import Tools.Error

data DriverReachedDestinationReq = DriverReachedDestinationReq
  { rideId :: Id DRide.Ride,
    driverId :: Id DP.Person,
    location :: LatLong,
    vehicleVariant :: VehicleVariant
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

driverReachedDestination :: DriverReachedDestinationReq -> Flow APISuccess
driverReachedDestination req = do
  let vehicleCategory = castVehicleVariantToVehicleCategory req.vehicleVariant
  case vehicleCategory of
    BUS -> do
      let tripTransactionId = cast @DRide.Ride @TripTransaction req.rideId
      tripTransaction <- QTT.findByTransactionId tripTransactionId >>= fromMaybeM (TripTransactionNotFound tripTransactionId.getId)
      fleetConfig <- QFC.findByPrimaryKey tripTransaction.fleetOwnerId >>= fromMaybeM (FleetConfigNotFound tripTransaction.fleetOwnerId.getId)
      WMB.endOngoingTripTransaction fleetConfig tripTransaction req.location AutoDetect False
    category -> throwError $ InvalidRequest ("Unsupported vehicle category, " <> show category)
  pure Success
