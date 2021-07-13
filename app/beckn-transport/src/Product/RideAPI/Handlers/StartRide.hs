module Product.RideAPI.Handlers.StartRide where

import qualified Beckn.Types.APISuccess as APISuccess
import Beckn.Types.Common
import Beckn.Types.Id
import EulerHS.Prelude
import Types.Error
import qualified Types.Storage.Person as Person
import qualified Types.Storage.ProductInstance as ProductInstance
import Utils.Common

data ServiceHandle m = ServiceHandle
  { findPersonById :: Id Person.Person -> m (Maybe Person.Person),
    findPIById :: Id ProductInstance.ProductInstance -> m (Maybe ProductInstance.ProductInstance),
    startRide :: Id ProductInstance.ProductInstance -> m (),
    notifyBAPRideStarted :: ProductInstance.ProductInstance -> ProductInstance.ProductInstance -> m (),
    rateLimitStartRide :: Id Person.Person -> Id ProductInstance.ProductInstance -> m ()
  }

startRideHandler :: (MonadThrow m, Log m) => ServiceHandle m -> Id Person.Person -> Id ProductInstance.ProductInstance -> Text -> m APISuccess.APISuccess
startRideHandler ServiceHandle {..} requestorId rideId otp = do
  rateLimitStartRide requestorId rideId
  requestor <- findPersonById requestorId >>= fromMaybeM PersonNotFound
  orderPi <- findPIById (cast rideId) >>= fromMaybeM PIDoesNotExist
  case requestor.role of
    Person.DRIVER -> do
      rideDriver <- orderPi.personId & fromMaybeM (PIFieldNotPresent "person")
      unless (rideDriver == requestorId) $ throwError NotAnExecutor
    _ -> throwError AccessDenied
  unless (isValidPiStatus (orderPi.status)) $ throwError $ PIInvalidStatus "This ride cannot be started"
  searchPiId <- orderPi.parentId & fromMaybeM (PIFieldNotPresent "parent_id")
  searchPi <- findPIById searchPiId >>= fromMaybeM PINotFound
  inAppOtp <- orderPi.udf4 & fromMaybeM (PIFieldNotPresent "udf4")
  when (otp /= inAppOtp) $ throwError IncorrectOTP
  logTagInfo "startRide" ("DriverId " <> getId requestorId <> ", RideId " <> getId rideId)
  startRide orderPi.id
  notifyBAPRideStarted searchPi orderPi{status = ProductInstance.INPROGRESS}
  pure APISuccess.Success
  where
    isValidPiStatus status = status `elem` [ProductInstance.CONFIRMED, ProductInstance.TRIP_ASSIGNED, ProductInstance.INSTOCK]
