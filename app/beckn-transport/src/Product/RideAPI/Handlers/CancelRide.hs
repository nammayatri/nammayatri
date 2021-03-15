{-# LANGUAGE OverloadedLabels #-}

module Product.RideAPI.Handlers.CancelRide where

import Beckn.Product.BusinessRule (BusinessRule, throwBusinessError)
import qualified Beckn.Types.APIResult as APIResult
import Beckn.Types.Id
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Person as Person
import Beckn.Types.Storage.ProductInstance (ProductInstance, ProductInstanceStatus (..))
import Data.Time (UTCTime)
import EulerHS.Prelude
import Types.App (Ride)

data ServiceHandle m = ServiceHandle
  { findPIById :: Id ProductInstance -> BusinessRule m ProductInstance,
    findPersonById :: Id Person.Person -> BusinessRule m Person.Person,
    cancelRide :: Id Ride -> Bool -> BusinessRule m (),
    generateGUID :: BusinessRule m Text,
    getCurrentTime :: BusinessRule m UTCTime
  }

cancelRideHandler :: Monad m => ServiceHandle m -> Text -> Text -> BusinessRule m APIResult.APIResult
cancelRideHandler ServiceHandle {..} authorizedEntityId rideId = do
  prodInst <- findPIById $ Id rideId
  unless (isValidPI prodInst) $ throwBusinessError "INVALID_PRODUCT_INSTANCE" "Invalid product instance."
  authPerson <- findPersonById $ Id authorizedEntityId
  case prodInst ^. #_personId of
    Nothing ->
      if authPerson ^. #_role == Person.ADMIN
        then cancelRide (Id rideId) False
        else throwBusinessError "NOT_AN_ORDER_EXECUTOR" "You are not an order executor."
    Just driverId ->
      if adminOrRideDriver authPerson driverId
        then cancelRide (Id rideId) True
        else throwBusinessError "NOT_AN_ORDER_EXECUTOR" "You are not an order executor."
  pure APIResult.Success
  where
    isValidPI prodInst =
      prodInst ^. #_type == Case.RIDEORDER
        && (prodInst ^. #_status) `elem` [CONFIRMED, INPROGRESS, TRIP_ASSIGNED, TRIP_REASSIGNMENT]
    adminOrRideDriver authPerson driverId =
      authPerson ^. #_role == Person.ADMIN
        || (authPerson ^. #_role == Person.DRIVER && authPerson ^. #_id == driverId)
