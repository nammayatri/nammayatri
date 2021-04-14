{-# LANGUAGE OverloadedLabels #-}

module Product.RideAPI.Handlers.StartRide where

import qualified Beckn.Types.APISuccess as APISuccess
import Beckn.Types.Common
import Beckn.Types.Id
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Person as Person
import qualified Beckn.Types.Storage.ProductInstance as ProductInstance
import Beckn.Utils.Common
import EulerHS.Prelude
import Types.Error

data ServiceHandle m = ServiceHandle
  { findPersonById :: Id Person.Person -> m Person.Person,
    findPIById :: Id ProductInstance.ProductInstance -> m ProductInstance.ProductInstance,
    findPIsByParentId :: Id ProductInstance.ProductInstance -> m [ProductInstance.ProductInstance],
    findCaseByIdsAndType :: [Id Case.Case] -> Case.CaseType -> m Case.Case,
    startRide :: [Id ProductInstance.ProductInstance] -> Id Case.Case -> Id Case.Case -> m (),
    notifyBAPRideStarted :: ProductInstance.ProductInstance -> ProductInstance.ProductInstance -> m ()
  }

startRideHandler :: (MonadThrow m, Log m) => ServiceHandle m -> Id Person.Person -> Id ProductInstance.ProductInstance -> Text -> m APISuccess.APISuccess
startRideHandler ServiceHandle {..} requestorId rideId otp = do
  requestor <- findPersonById requestorId
  orderPi <- findPIById $ cast rideId
  case requestor ^. #_role of
    Person.DRIVER -> do
      rideDriver <- orderPi ^. #_personId & fromMaybeM PIInvalidStatus
      unless (rideDriver == requestorId) $ throwError NotAnExecutor
    _ -> throwError AccessDenied
  unless (isValidPiStatus (orderPi ^. #_status)) $ throwError PIInvalidStatus
  searchPiId <- orderPi ^. #_parentId & fromMaybeM PIParentIdNotPresent
  searchPi <- findPIById searchPiId
  inAppOtp <- orderPi ^. #_udf4 & fromMaybeM PIOTPNotPresent
  when (otp /= inAppOtp) $ throwError IncorrectOTP
  piList <- findPIsByParentId searchPiId
  trackerCase <- findCaseByIdsAndType (ProductInstance._caseId <$> piList) Case.LOCATIONTRACKER
  orderCase <- findCaseByIdsAndType (ProductInstance._caseId <$> piList) Case.RIDEORDER
  startRide (ProductInstance._id <$> piList) (Case._id trackerCase) (Case._id orderCase)
  notifyBAPRideStarted searchPi orderPi
  pure APISuccess.Success
  where
    isValidPiStatus status = status `elem` [ProductInstance.CONFIRMED, ProductInstance.TRIP_ASSIGNED, ProductInstance.INSTOCK]
