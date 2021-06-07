module Product.RideAPI.Handlers.EndRide where

import qualified Beckn.Types.APISuccess as APISuccess
import Beckn.Types.Common
import Beckn.Types.Id
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Person as Person
import qualified Beckn.Types.Storage.ProductInstance as PI
import EulerHS.Prelude
import Types.App (Driver)
import Types.Error
import Utils.Common

data ServiceHandle m = ServiceHandle
  { findPersonById :: Id Person.Person -> m Person.Person,
    findPIById :: Id PI.ProductInstance -> m (Maybe PI.ProductInstance),
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
  orderPi <- findPIById (cast rideId) >>= fromMaybeM PIDoesNotExist
  driverId <- orderPi.personId & fromMaybeM (PIFieldNotPresent "person")
  case requestor.role of
    Person.DRIVER -> unless (requestorId == driverId) $ throwError NotAnExecutor
    _ -> throwError AccessDenied
  unless (orderPi.status == PI.INPROGRESS) $ throwError $ PIInvalidStatus "This ride cannot be ended"

  searchPiId <- orderPi.parentId & fromMaybeM (PIFieldNotPresent "parent_id")
  searchPi <- findPIById searchPiId >>= fromMaybeM PINotFound
  piList <- findAllPIByParentId searchPiId
  trackerCase <- findCaseByIdAndType (PI.caseId <$> piList) Case.LOCATIONTRACKER
  orderCase <- findCaseByIdAndType (PI.caseId <$> piList) Case.RIDEORDER

  endRideTransaction (PI.id <$> piList) (trackerCase.id) (orderCase.id) (cast driverId)

  notifyUpdateToBAP searchPi orderPi PI.COMPLETED

  return APISuccess.Success
