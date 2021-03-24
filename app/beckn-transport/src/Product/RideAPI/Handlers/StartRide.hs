{-# LANGUAGE OverloadedLabels #-}

module Product.RideAPI.Handlers.StartRide where

import qualified Beckn.Types.APIResult as APIResult
import Beckn.Types.Id
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Person as Person
import qualified Beckn.Types.Storage.ProductInstance as ProductInstance
import Beckn.Utils.Common (fromMaybeMWithInfo400, throwErrorWithInfo400)
import Beckn.Utils.Logging (Log)
import EulerHS.Prelude

data ServiceHandle m = ServiceHandle
  { findPersonById :: Id Person.Person -> m Person.Person,
    findPIById :: Id ProductInstance.ProductInstance -> m ProductInstance.ProductInstance,
    findPIsByParentId :: Id ProductInstance.ProductInstance -> m [ProductInstance.ProductInstance],
    findCaseByIdsAndType :: [Id Case.Case] -> Case.CaseType -> m Case.Case,
    startRide :: [Id ProductInstance.ProductInstance] -> Id Case.Case -> Id Case.Case -> m (),
    notifyBAPRideStarted :: ProductInstance.ProductInstance -> ProductInstance.ProductInstance -> m ()
  }

startRideHandler :: (MonadThrow m, Log m) => ServiceHandle m -> Text -> Text -> Text -> m APIResult.APIResult
startRideHandler ServiceHandle {..} requestorId rideId otp = do
  requestor <- findPersonById $ Id requestorId
  orderPi <- findPIById $ Id rideId
  unless (requestor ^. #_role == Person.ADMIN) do
    rideDriver <- orderPi ^. #_personId & fromMaybeMWithInfo400 "NOT_AN_EXECUTOR_OF_THIS_RIDE" "You are not an executor of this ride."
    when (rideDriver /= Id requestorId || requestor ^. #_role /= Person.DRIVER) $
      throwErrorWithInfo400 "NOT_AN_EXECUTOR_OF_THIS_RIDE" "You are not an executor of this ride."
  unless (isValidPiStatus (orderPi ^. #_status)) $ throwErrorWithInfo400 "INVALID_RIDE_STATUS" "Ride cannot be started."
  searchPiId <- orderPi ^. #_parentId & fromMaybeMWithInfo400 "INVALID_RIDE_ID" "Invalid ride id."
  searchPi <- findPIById searchPiId
  inAppOtp <- orderPi ^. #_udf4 & fromMaybeMWithInfo400 "RIDE_OTP_MISSING" "Ride does not have OTP."
  when (otp /= inAppOtp) $ throwErrorWithInfo400 "INCORRECT_TRIP_OTP" "Input OTP is wrong."
  piList <- findPIsByParentId searchPiId
  trackerCase <- findCaseByIdsAndType (ProductInstance._caseId <$> piList) Case.LOCATIONTRACKER
  orderCase <- findCaseByIdsAndType (ProductInstance._caseId <$> piList) Case.RIDEORDER
  startRide (ProductInstance._id <$> piList) (Case._id trackerCase) (Case._id orderCase)
  notifyBAPRideStarted searchPi orderPi
  pure APIResult.Success
  where
    isValidPiStatus status = status `elem` [ProductInstance.CONFIRMED, ProductInstance.TRIP_ASSIGNED, ProductInstance.INSTOCK]
