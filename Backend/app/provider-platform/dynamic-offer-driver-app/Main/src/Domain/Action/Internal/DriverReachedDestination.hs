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
import qualified SharedLogic.External.LocationTrackingService.Flow as LF
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.TripTransaction as QTT
import qualified Storage.Queries.Vehicle as QV
import Tools.Error
import qualified Tools.Notifications as TN

data DriverReachedDestinationReq = DriverReachedDestinationReq
  { rideId :: Id DRide.Ride,
    driverId :: Id DP.Person,
    location :: LatLong,
    vehicleVariant :: VehicleVariant
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

driverReachedDestination :: DriverReachedDestinationReq -> Flow APISuccess
driverReachedDestination req = do
  -- End The TripTransaction with Notification for Bus VehicleCategory
  let vehicleCategory = castVehicleVariantToVehicleCategory req.vehicleVariant
  case vehicleCategory of
    BUS -> do
      let tripTransactionId = cast @DRide.Ride @TripTransaction req.rideId
      tripTransaction <- QTT.findByTransactionId tripTransactionId >>= fromMaybeM (InternalError "no trip transaction found")
      advancedTripTransaction <- findNextEligibleTripTransactionByDriverIdStatus req.driverId TRIP_ASSIGNED
      void $ LF.rideEnd (cast tripTransaction.id) req.location.lat req.location.lon tripTransaction.merchantId req.driverId (advancedTripTransaction <&> (cast . (.id)))
      QDI.updateOnRide False req.driverId
      QTT.updateStatus COMPLETED (Just req.location) tripTransactionId
      QV.deleteByDriverid req.driverId
      TN.notifyWmbOnRide req.driverId tripTransaction.merchantOperatingCityId COMPLETED "Ride Ended" "Your ride has ended" Nothing
    category -> throwError $ InvalidRequest ("Unsupported vehicle category, " <> show category)
  pure Success
  where
    findNextEligibleTripTransactionByDriverIdStatus driverId status =
      QTT.findAllTripTransactionByDriverIdStatus driverId (Just 1) (Just 0) (Just status) >>= \case
        (trip : _) -> pure (Just trip)
        [] -> pure Nothing
