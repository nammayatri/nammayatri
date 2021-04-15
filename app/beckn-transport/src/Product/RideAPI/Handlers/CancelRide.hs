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

type MonadHandler m = (MonadThrow m, Log m)

data ServiceHandle m = ServiceHandle
  { findPIById :: Id ProductInstance -> m ProductInstance,
    findPersonById :: Id Person.Person -> m Person.Person,
    cancelRide :: Id Ride -> Bool -> m ()
  }

cancelRideHandler :: MonadHandler m => ServiceHandle m -> Text -> Id Ride -> m APISuccess.APISuccess
cancelRideHandler ServiceHandle {..} authorizedEntityId rideId = do
  prodInst <- findPIById $ cast rideId
  unless (isValidPI prodInst) $ throwError PIInvalidStatus
  authPerson <- findPersonById $ Id authorizedEntityId
  case authPerson ^. #_role of
    Person.ADMIN -> cancelRide rideId False
    Person.DRIVER -> do
      driverId <- prodInst ^. #_personId & fromMaybeM PIInvalidStatus
      unless (authPerson ^. #_id == driverId) $ throwError NotAnExecutor
      cancelRide rideId True
    _ -> throwError AccessDenied
  pure APISuccess.Success
  where
    isValidPI prodInst =
      prodInst ^. #_type == Case.RIDEORDER
        && (prodInst ^. #_status) `elem` [CONFIRMED, TRIP_ASSIGNED, TRIP_REASSIGNMENT]
