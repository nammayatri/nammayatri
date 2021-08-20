module Product.RideAPI.Handlers.CancelRide where

import qualified Beckn.Types.APISuccess as APISuccess
import Beckn.Types.Common
import Beckn.Types.Id
import qualified Beckn.Types.Mobility.Order as Mobility
import EulerHS.Prelude
import Types.API.Ride (CancelRideReq (..))
import Types.App (Ride)
import Types.Error
import qualified Types.Storage.Case as Case
import qualified Types.Storage.Person as Person
import qualified Types.Storage.Person as SP
import Types.Storage.ProductInstance (ProductInstance, ProductInstanceStatus (..))
import qualified Types.Storage.RideCancellationReason as SRCR
import Utils.Common

type MonadHandler m = (MonadThrow m, Log m)

data ServiceHandle m = ServiceHandle
  { findPIById :: Id ProductInstance -> m (Maybe ProductInstance),
    findPersonById :: Id Person.Person -> m (Maybe Person.Person),
    cancelRide :: Id Ride -> SRCR.RideCancellationReason -> m ()
  }

cancelRideHandler :: MonadHandler m => ServiceHandle m -> Id SP.Person -> Id Ride -> CancelRideReq -> m APISuccess.APISuccess
cancelRideHandler ServiceHandle {..} personId rideId req = do
  prodInst <- findPIById (cast rideId) >>= fromMaybeM PIDoesNotExist
  unless (isValidPI prodInst) $ throwError $ PIInvalidStatus "This ride cannot be canceled"
  authPerson <-
    findPersonById personId
      >>= fromMaybeM PersonNotFound
  case authPerson.role of
    Person.ADMIN -> cancelRide rideId $ rideCancelationReason Mobility.ByOrganization
    Person.DRIVER -> do
      driverId <- prodInst.personId & fromMaybeM (PIFieldNotPresent "person")
      unless (authPerson.id == driverId) $ throwError NotAnExecutor
      cancelRide rideId $ rideCancelationReason Mobility.ByDriver
  pure APISuccess.Success
  where
    isValidPI prodInst =
      prodInst._type == Case.RIDEORDER
        && (prodInst.status) `elem` [CONFIRMED, TRIP_ASSIGNED, TRIP_REASSIGNMENT]
    rideCancelationReason source = do
      let CancelRideReq {..} = req
      SRCR.RideCancellationReason
        { rideId = cast rideId,
          source = source,
          reasonCode = Just reasonCode,
          ..
        }
