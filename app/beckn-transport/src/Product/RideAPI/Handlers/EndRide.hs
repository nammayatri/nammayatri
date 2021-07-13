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
    endRideTransaction :: Id PI.ProductInstance -> Id Driver -> Amount -> m (),
    findCaseById :: Id Case.Case -> m (Maybe Case.Case),
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
  _case <- findCaseById searchPi.caseId >>= fromMaybeM CaseNotFound
  logTagInfo "endRide" ("DriverId " <> getId requestorId <> ", RideId " <> getId rideId)

  actualPrice <-
    ifM
      recalculateFareEnabled
      (recalculateFare _case orderPi)
      (orderPi.price & fromMaybeM (PIFieldNotPresent "price"))

  endRideTransaction orderPi.id (cast driverId) actualPrice

  notifyUpdateToBAP (updateActualPrice actualPrice searchPi) (updateActualPrice actualPrice orderPi){status = PI.COMPLETED} PI.COMPLETED

  return APISuccess.Success
  where
    recalculateFare _case orderPi = do
      transporterId <- Id <$> _case.provider & fromMaybeM (CaseFieldNotPresent "provider")
      vehicleVariant <-
        (_case.udf1 >>= readMaybe . T.unpack)
          & fromMaybeM (CaseFieldNotPresent "udf1")
      oldDistance <-
        (_case.udf5 >>= readMaybe . T.unpack)
          & fromMaybeM (CaseFieldNotPresent "udf5")
      fare <- calculateFare transporterId vehicleVariant orderPi.distance _case.startTime
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
    updateActualPrice :: Amount -> PI.ProductInstance -> PI.ProductInstance
    updateActualPrice = \p pi -> pi {PI.actualPrice = Just p}
