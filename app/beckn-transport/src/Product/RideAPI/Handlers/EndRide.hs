{-# LANGUAGE OverloadedLabels #-}

module Product.RideAPI.Handlers.EndRide where

import qualified Beckn.Types.APISuccess as APISuccess
import Beckn.Types.Common
import Beckn.Types.Id
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Person as Person
import qualified Beckn.Types.Storage.ProductInstance as PI
import Beckn.Utils.Common
import EulerHS.Prelude
import qualified EulerHS.Types as ET
import Types.App (Driver)
import Types.Error

data ServiceHandle m = ServiceHandle
  { findPersonById :: Id Person.Person -> m Person.Person,
    findPIById :: Id PI.ProductInstance -> m (ET.DBResult (Maybe PI.ProductInstance)),
    findAllPIByParentId :: Id PI.ProductInstance -> m [PI.ProductInstance],
    endRideTransaction :: [Id PI.ProductInstance] -> Id Case.Case -> Id Case.Case -> Id Driver -> m (),
    findCaseByIdAndType :: [Id Case.Case] -> Case.CaseType -> m Case.Case,
    notifyUpdateToBAP :: PI.ProductInstance -> PI.ProductInstance -> PI.ProductInstanceStatus -> m ()
  }

endRideHandler ::
  (MonadThrow m, Log m) =>
  ServiceHandle m ->
  Id Person.Person ->
  Id PI.ProductInstance ->
  m APISuccess.APISuccess
endRideHandler ServiceHandle {..} requestorId rideId = do
  requestor <- findPersonById requestorId
  orderPi <- findPIById (cast rideId) >>= (`checkDBErrorOrEmpty` PIInvalidId)
  driverId <- orderPi ^. #_personId & fromMaybeM PIPersonNotPresent
  case requestor ^. #_role of
    Person.DRIVER -> unless (requestorId == driverId) $ throwError NotAnExecutor
    _ -> throwError AccessDenied
  unless (orderPi ^. #_status == PI.INPROGRESS) $ throwError PIInvalidStatus

  searchPiId <- orderPi ^. #_parentId & fromMaybeM PIParentIdNotPresent
  searchPi <- findPIById searchPiId >>= (`checkDBErrorOrEmpty` PINotFound)
  piList <- findAllPIByParentId searchPiId
  trackerCase <- findCaseByIdAndType (PI._caseId <$> piList) Case.LOCATIONTRACKER
  orderCase <- findCaseByIdAndType (PI._caseId <$> piList) Case.RIDEORDER

  endRideTransaction (PI._id <$> piList) (trackerCase ^. #_id) (orderCase ^. #_id) (cast driverId)

  notifyUpdateToBAP searchPi orderPi PI.COMPLETED

  return APISuccess.Success
