module Product.RideAPI.Handlers.EndRide where

import qualified Beckn.Types.APISuccess as APISuccess
import Beckn.Types.Amount
import Beckn.Types.Common
import Beckn.Types.Id
import qualified Data.Text as T
import Data.Time (UTCTime)
import EulerHS.Prelude hiding (pi)
import Types.App (Driver)
import Types.Error
import qualified Types.Storage.Case as Case
import Types.Storage.Organization (Organization)
import qualified Types.Storage.Person as Person
import qualified Types.Storage.ProductInstance as PI
import qualified Types.Storage.Vehicle as Vehicle
import Utils.Common

data ServiceHandle m = ServiceHandle
  { findPersonById :: Id Person.Person -> m (Maybe Person.Person),
    findPIById :: Id PI.ProductInstance -> m (Maybe PI.ProductInstance),
    findAllPIByParentId :: Id PI.ProductInstance -> m [PI.ProductInstance],
    endRideTransaction :: [Id PI.ProductInstance] -> Id Case.Case -> Id Case.Case -> Id Driver -> Maybe Amount -> m (),
    findCaseByIdAndType :: [Id Case.Case] -> Case.CaseType -> m (Maybe Case.Case),
    notifyUpdateToBAP :: PI.ProductInstance -> PI.ProductInstance -> PI.ProductInstanceStatus -> m (),
    calculateFare ::
      Id Organization ->
      Vehicle.Variant ->
      Double ->
      UTCTime ->
      m Amount,
    recalculateFareEnabled :: m Bool,
    putDiffMetric :: Amount -> Double -> m ()
  }

endRideHandler ::
  (MonadThrow m, Log m) =>
  ServiceHandle m ->
  Id Person.Person ->
  Id PI.ProductInstance ->
  m APISuccess.APISuccess
endRideHandler ServiceHandle {..} requestorId rideId = do
  requestor <- findPersonById requestorId >>= fromMaybeM PersonNotFound
  orderPi <- findPIById (cast rideId) >>= fromMaybeM PIDoesNotExist
  driverId <- orderPi.personId & fromMaybeM (PIFieldNotPresent "person")
  case requestor.role of
    Person.DRIVER -> unless (requestorId == driverId) $ throwError NotAnExecutor
    _ -> throwError AccessDenied
  unless (orderPi.status == PI.INPROGRESS) $ throwError $ PIInvalidStatus "This ride cannot be ended"

  searchPiId <- orderPi.parentId & fromMaybeM (PIFieldNotPresent "parent_id")
  searchPi <- findPIById searchPiId >>= fromMaybeM PINotFound
  piList <- findAllPIByParentId searchPiId
  trackerCase <- findCaseByIdAndType (PI.caseId <$> piList) Case.LOCATIONTRACKER >>= fromMaybeM CaseNotFound
  orderCase <- findCaseByIdAndType (PI.caseId <$> piList) Case.RIDEORDER >>= fromMaybeM CaseNotFound
  logTagInfo "endRide" ("DriverId " <> getId requestorId <> ", RideId " <> getId rideId)

  mbFare <-
    ifM
      recalculateFareEnabled
      (Just <$> recalculateFare orderCase orderPi)
      (pure Nothing)

  endRideTransaction (PI.id <$> piList) (trackerCase.id) (orderCase.id) (cast driverId) mbFare

  notifyUpdateToBAP
    (updatePriceMb mbFare searchPi)
    (updatePriceMb mbFare orderPi)
    PI.COMPLETED

  return APISuccess.Success
  where
    recalculateFare orderCase orderPi = do
      transporterId <- Id <$> orderCase.provider & fromMaybeM (CaseFieldNotPresent "provider")
      vehicleVariant <-
        (orderCase.udf1 >>= readMaybe . T.unpack)
          & fromMaybeM (CaseFieldNotPresent "udf1")
      oldDistance <-
        (orderCase.udf5 >>= readMaybe . T.unpack)
          & fromMaybeM (CaseFieldNotPresent "udf5")
      fare <- calculateFare transporterId vehicleVariant orderPi.distance orderCase.startTime
      let distanceDiff = orderPi.distance - oldDistance
      price <- orderPi.price & fromMaybeM (PIFieldNotPresent "price")
      let fareDiff = fare - price
      logTagInfo "Fare recalculation" $
        "Fare difference: "
          <> show (amountToDouble fareDiff)
          <> ", Distance difference: "
          <> show distanceDiff
      putDiffMetric fareDiff distanceDiff
      pure fare
    updatePriceMb :: Maybe Amount -> PI.ProductInstance -> PI.ProductInstance
    updatePriceMb = maybe identity $ \p pi -> pi {PI.price = Just p}
