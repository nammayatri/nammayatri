{-# LANGUAGE OverloadedLabels #-}

module Product.RideAPI.Handlers.CancelRide where

import qualified Beckn.Types.APISuccess as APISuccess
import Beckn.Types.Common
import Beckn.Types.Id
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Person as Person
import Beckn.Types.Storage.ProductInstance (ProductInstance, ProductInstanceStatus (..))
import Beckn.Utils.Common
import EulerHS.Prelude
import Types.App (Ride)
import Types.Error

data ServiceHandle m = ServiceHandle
  { findPIById :: Id ProductInstance -> m ProductInstance,
    findPersonById :: Id Person.Person -> m Person.Person,
    cancelRide :: Id Ride -> Bool -> m ()
  }

cancelRideHandler :: MonadHandler m => ServiceHandle m -> Text -> Text -> m APISuccess.APISuccess
cancelRideHandler ServiceHandle {..} authorizedEntityId rideId = do
  prodInst <- findPIById $ Id rideId
  unless (isValidPI prodInst) $ throwError PIInvalidStatus
  authPerson <- findPersonById $ Id authorizedEntityId
  case prodInst ^. #_personId of
    Nothing ->
      if authPerson ^. #_role == Person.ADMIN
        then cancelRide (Id rideId) False
        else throwError NotAnExecutor
    Just driverId ->
      if adminOrRideDriver authPerson driverId
        then cancelRide (Id rideId) True
        else throwError NotAnExecutor
  pure APISuccess.Success
  where
    isValidPI prodInst =
      prodInst ^. #_type == Case.RIDEORDER
        && (prodInst ^. #_status) `elem` [CONFIRMED, TRIP_ASSIGNED, TRIP_REASSIGNMENT]
    adminOrRideDriver authPerson driverId =
      authPerson ^. #_role == Person.ADMIN
        || (authPerson ^. #_role == Person.DRIVER && authPerson ^. #_id == driverId)
