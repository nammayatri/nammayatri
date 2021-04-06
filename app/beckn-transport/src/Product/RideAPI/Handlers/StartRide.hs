{-# LANGUAGE OverloadedLabels #-}

module Product.RideAPI.Handlers.StartRide where

import Beckn.TypeClass.IsAPIError
import qualified Beckn.Types.APISuccess as APISuccess
import Beckn.Types.Common
import Beckn.Types.Id
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Person as Person
import qualified Beckn.Types.Storage.ProductInstance as ProductInstance
import Beckn.Utils.Common
import EulerHS.Prelude

data ServiceHandle m = ServiceHandle
  { findPersonById :: Id Person.Person -> m Person.Person,
    findPIById :: Id ProductInstance.ProductInstance -> m ProductInstance.ProductInstance,
    findPIsByParentId :: Id ProductInstance.ProductInstance -> m [ProductInstance.ProductInstance],
    findCaseByIdsAndType :: [Id Case.Case] -> Case.CaseType -> m Case.Case,
    startRide :: [Id ProductInstance.ProductInstance] -> Id Case.Case -> Id Case.Case -> m (),
    notifyBAPRideStarted :: ProductInstance.ProductInstance -> ProductInstance.ProductInstance -> m ()
  }

data StartRideError
  = NotAnExecutor
  | InvalidRideStatus
  | InvalidRideId
  | RideMissingOTP
  | IncorrectOTP
  deriving (Eq, Show)

instance IsAPIError StartRideError where
  toAPIError NotAnExecutor = APIError "NOT_AN_EXECUTOR_OF_THIS_RIDE" "You are not an executor of this ride."
  toAPIError InvalidRideStatus = APIError "INVALID_RIDE_STATUS" "Ride cannot be started."
  toAPIError InvalidRideId = APIError "INVALID_RIDE_ID" "Invalid ride id."
  toAPIError RideMissingOTP = APIError "RIDE_OTP_MISSING" "Ride does not have OTP."
  toAPIError IncorrectOTP = APIError "INCORRECT_RIDE_OTP" "Input OTP is wrong."
  toStatusCode NotAnExecutor = E400
  toStatusCode InvalidRideStatus = E400
  toStatusCode InvalidRideId = E400
  toStatusCode RideMissingOTP = E500
  toStatusCode IncorrectOTP = E400

startRideHandler :: (MonadThrow m, Log m) => ServiceHandle m -> Text -> Text -> Text -> m APISuccess.APISuccess
startRideHandler ServiceHandle {..} requestorId rideId otp = do
  requestor <- findPersonById $ Id requestorId
  orderPi <- findPIById $ Id rideId
  unless (requestor ^. #_role == Person.ADMIN) do
    rideDriver <- orderPi ^. #_personId & fromMaybeM NotAnExecutor
    when (rideDriver /= Id requestorId || requestor ^. #_role /= Person.DRIVER) $
      throwError NotAnExecutor
  unless (isValidPiStatus (orderPi ^. #_status)) $ throwError InvalidRideStatus
  searchPiId <- orderPi ^. #_parentId & fromMaybeM InvalidRideId
  searchPi <- findPIById searchPiId
  inAppOtp <- orderPi ^. #_udf4 & fromMaybeM RideMissingOTP
  when (otp /= inAppOtp) $ throwError IncorrectOTP
  piList <- findPIsByParentId searchPiId
  trackerCase <- findCaseByIdsAndType (ProductInstance._caseId <$> piList) Case.LOCATIONTRACKER
  orderCase <- findCaseByIdsAndType (ProductInstance._caseId <$> piList) Case.RIDEORDER
  startRide (ProductInstance._id <$> piList) (Case._id trackerCase) (Case._id orderCase)
  notifyBAPRideStarted searchPi orderPi
  pure APISuccess.Success
  where
    isValidPiStatus status = status `elem` [ProductInstance.CONFIRMED, ProductInstance.TRIP_ASSIGNED, ProductInstance.INSTOCK]
