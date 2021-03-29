{-# LANGUAGE OverloadedLabels #-}

module Product.RideAPI.Handlers.CancelRide where

import Beckn.TypeClass.IsAPIError
import qualified Beckn.Types.APIResult as APIResult
import Beckn.Types.Common (Log)
import Beckn.Types.Id
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Person as Person
import Beckn.Types.Storage.ProductInstance (ProductInstance, ProductInstanceStatus (..))
import Beckn.Utils.Common
import Data.Time (UTCTime)
import EulerHS.Prelude
import Types.App (Ride)

data ServiceHandle m = ServiceHandle
  { findPIById :: Id ProductInstance -> m ProductInstance,
    findPersonById :: Id Person.Person -> m Person.Person,
    cancelRide :: Id Ride -> Bool -> m (),
    generateGUID :: m Text,
    getCurrentTime :: m UTCTime
  }

data CancelRideError
  = InvalidRideId
  | NotAnExecutor
  deriving (Eq, Show)

instance IsAPIError CancelRideError where
  toAPIError InvalidRideId = APIError "INVALID_RIDE_ID" "Invalid ride id."
  toAPIError NotAnExecutor = APIError "NOT_AN_EXECUTOR_OF_THIS_RIDE" "You are not an executor of this ride."
  toStatusCode InvalidRideId = E400
  toStatusCode NotAnExecutor = E400

cancelRideHandler :: (MonadThrow m, Log m) => ServiceHandle m -> Text -> Text -> m APIResult.APIResult
cancelRideHandler ServiceHandle {..} authorizedEntityId rideId = do
  prodInst <- findPIById $ Id rideId
  unless (isValidPI prodInst) $ throwError InvalidRideId
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
  pure APIResult.Success
  where
    isValidPI prodInst =
      prodInst ^. #_type == Case.RIDEORDER
        && (prodInst ^. #_status) `elem` [CONFIRMED, TRIP_ASSIGNED, TRIP_REASSIGNMENT]
    adminOrRideDriver authPerson driverId =
      authPerson ^. #_role == Person.ADMIN
        || (authPerson ^. #_role == Person.DRIVER && authPerson ^. #_id == driverId)
