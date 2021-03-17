{-# LANGUAGE OverloadedLabels #-}

module Product.RideAPI.Handlers.StartRide where

import qualified Beckn.Types.APIResult as APIResult
import Beckn.Types.App (CaseId, PersonId (..), ProductInstanceId (..))
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Person as Person
import qualified Beckn.Types.Storage.ProductInstance as ProductInstance
import Beckn.Utils.Common (fromMaybeThrowM400, throwM400)
import EulerHS.Prelude

data ServiceHandle m = ServiceHandle
  { findPersonById :: PersonId -> m Person.Person,
    findPIById :: ProductInstanceId -> m ProductInstance.ProductInstance,
    findPIsByParentId :: ProductInstanceId -> m [ProductInstance.ProductInstance],
    findCaseByIdsAndType :: [CaseId] -> Case.CaseType -> m Case.Case,
    updatePIsStatus :: [ProductInstanceId] -> ProductInstance.ProductInstanceStatus -> m (),
    updateCaseStatus :: CaseId -> Case.CaseStatus -> m (),
    notifyBAPRideStarted :: ProductInstance.ProductInstance -> ProductInstance.ProductInstance -> m ()
  }

startRideHandler :: MonadThrow m => ServiceHandle m -> Text -> Text -> Text -> m APIResult.APIResult
startRideHandler ServiceHandle {..} requestorId rideId otp = do
  requestor <- findPersonById $ PersonId requestorId
  orderPi <- findPIById $ ProductInstanceId rideId
  unless (requestor ^. #_role == Person.ADMIN) do
    rideDriver <- orderPi ^. #_personId & fromMaybeThrowM400 "NOT_AN_ORDER_EXECUTOR"
    when (rideDriver /= PersonId requestorId) do
      _ <- throwM400 "NOT_AN_ORDER_EXECUTOR"
      pure ()
  whenLeft (ProductInstance.validateStatusTransition (orderPi ^. #_status) ProductInstance.INPROGRESS) $
    \_ -> throwM400 "INVALID_ORDER_STATUS"
  searchPiId <- orderPi ^. #_parentId & fromMaybeThrowM400 "INVALID_RIDE_ID"
  searchPi <- findPIById searchPiId
  inAppOtp <- orderPi ^. #_udf4 & fromMaybeThrowM400 "IN_APP_OTP_MISSING"
  if otp == inAppOtp
    then do
      piList <- findPIsByParentId searchPiId
      trackerCase <- findCaseByIdsAndType (ProductInstance._caseId <$> piList) Case.LOCATIONTRACKER
      orderCase <- findCaseByIdsAndType (ProductInstance._caseId <$> piList) Case.RIDEORDER
      _ <- updatePIsStatus (ProductInstance._id <$> piList) ProductInstance.INPROGRESS
      updateCaseStatus (Case._id trackerCase) Case.INPROGRESS
      updateCaseStatus (Case._id orderCase) Case.INPROGRESS
    else throwM400 "INCORRECT_TRIP_OTP"
  notifyBAPRideStarted searchPi orderPi
  pure APIResult.Success
