{-# LANGUAGE OverloadedLabels #-}

module Product.RideAPI.Handlers.StartRide where

import qualified Beckn.Types.APIResult as APIResult
import Beckn.Types.Id
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Person as Person
import qualified Beckn.Types.Storage.ProductInstance as ProductInstance
import Beckn.Utils.Common (fromMaybeThrowM400, throwM400)
import EulerHS.Prelude

data ServiceHandle m = ServiceHandle
  { findPersonById :: Id Person.Person -> m Person.Person,
    findPIById :: Id ProductInstance.ProductInstance -> m ProductInstance.ProductInstance,
    findPIsByParentId :: Id ProductInstance.ProductInstance -> m [ProductInstance.ProductInstance],
    findCaseByIdsAndType :: [Id Case.Case] -> Case.CaseType -> m Case.Case,
    startRide :: [Id ProductInstance.ProductInstance] -> Id Case.Case -> Id Case.Case -> m (),
    notifyBAPRideStarted :: ProductInstance.ProductInstance -> ProductInstance.ProductInstance -> m ()
  }

startRideHandler :: MonadThrow m => ServiceHandle m -> Text -> Text -> Text -> m APIResult.APIResult
startRideHandler ServiceHandle {..} requestorId rideId otp = do
  requestor <- findPersonById $ Id requestorId
  orderPi <- findPIById $ Id rideId
  unless (requestor ^. #_role == Person.ADMIN) do
    rideDriver <- orderPi ^. #_personId & fromMaybeThrowM400 "NOT_AN_ORDER_EXECUTOR"
    when (rideDriver /= Id requestorId || requestor ^. #_role /= Person.DRIVER && requestor ^. #_role /= Person.ADMIN) do
      _ <- throwM400 "NOT_AN_ORDER_EXECUTOR"
      pure ()
  whenLeft (ProductInstance.validateStatusTransition (orderPi ^. #_status) ProductInstance.INPROGRESS) $
    \_ -> throwM400 "INVALID_ORDER_STATUS"
  searchPiId <- orderPi ^. #_parentId & fromMaybeThrowM400 "INVALID_RIDE_ID"
  searchPi <- findPIById searchPiId
  inAppOtp <- orderPi ^. #_udf4 & fromMaybeThrowM400 "RIDE_OTP_MISSING"
  when (otp /= inAppOtp) $ throwM400 "INCORRECT_TRIP_OTP"
  piList <- findPIsByParentId searchPiId
  trackerCase <- findCaseByIdsAndType (ProductInstance._caseId <$> piList) Case.LOCATIONTRACKER
  orderCase <- findCaseByIdsAndType (ProductInstance._caseId <$> piList) Case.RIDEORDER
  startRide (ProductInstance._id <$> piList) (Case._id trackerCase) (Case._id orderCase)
  notifyBAPRideStarted searchPi orderPi
  pure APIResult.Success
