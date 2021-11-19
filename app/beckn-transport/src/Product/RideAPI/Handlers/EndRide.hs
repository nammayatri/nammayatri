module Product.RideAPI.Handlers.EndRide where

import qualified Beckn.Types.APISuccess as APISuccess
import Beckn.Types.Amount
import Beckn.Types.Common
import Beckn.Types.Id
import qualified Data.Text as T
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime)
import EulerHS.Prelude hiding (pi)
import qualified Product.FareCalculator.Interpreter as Fare
import Product.Location (missingLocationUpdatesKey)
import Types.App (Driver)
import Types.Error
import qualified Types.Storage.Case as Case
import qualified Types.Storage.DriverLocation as DrLoc
import Types.Storage.Organization (Organization)
import qualified Types.Storage.Person as Person
import qualified Types.Storage.ProductInstance as PI
import qualified Types.Storage.Vehicle as Vehicle
import Utils.Common

data ServiceHandle m = ServiceHandle
  { findPersonById :: Id Person.Person -> m (Maybe Person.Person),
    findPIById :: Id PI.ProductInstance -> m (Maybe PI.ProductInstance),
    findAllPIByParentId :: Id PI.ProductInstance -> m [PI.ProductInstance],
    endRideTransaction :: [Id PI.ProductInstance] -> Id Case.Case -> Id Case.Case -> Id Driver -> Amount -> Amount -> Double -> m (),
    findCaseByIdAndType :: [Id Case.Case] -> Case.CaseType -> m (Maybe Case.Case),
    notifyUpdateToBAP :: PI.ProductInstance -> PI.ProductInstance -> PI.ProductInstanceStatus -> m (),
    calculateFare ::
      Id Organization ->
      Vehicle.Variant ->
      Double ->
      UTCTime ->
      m Fare.FareParameters,
    recalculateFareEnabled :: m Bool,
    putDiffMetric :: Amount -> Double -> m (),
    findDriverLocById :: Id Person.Person -> m (Maybe DrLoc.DriverLocation),
    getKeyRedis :: Text -> m (Maybe ()),
    updateLocationAllowedDelay :: m NominalDiffTime
  }

endRideHandler ::
  (MonadThrow m, Log m, MonadTime m) =>
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

  (chargeableDistance, fare, totalFare) <- recalculateFare orderCase orderPi

  endRideTransaction (PI.id <$> piList) (trackerCase.id) (orderCase.id) (cast driverId) fare totalFare chargeableDistance
  notifyUpdateToBAP
    searchPi{chargeableDistance = Just chargeableDistance, fare = Just fare, totalFare = Just totalFare}
    orderPi{chargeableDistance = Just chargeableDistance, fare = Just fare, totalFare = Just totalFare}
    PI.COMPLETED

  return APISuccess.Success
  where
    lastLocUdateTooLongAgo = do
      now <- getCurrentTime
      allowedUpdatesDelay <- updateLocationAllowedDelay
      findDriverLocById requestorId
        <&> maybe True (\loc -> now `diffUTCTime` loc.updatedAt > allowedUpdatesDelay)

    thereWereMissingLocUpdates =
      getKeyRedis (missingLocationUpdatesKey rideId)
        <&> isJust

    recalculateFare orderCase orderPi = do
      transporterId <- Id <$> orderCase.provider & fromMaybeM (CaseFieldNotPresent "provider")
      oldDistance <-
        (orderCase.udf5 >>= readMaybe . T.unpack)
          & fromMaybeM (CaseFieldNotPresent "udf5")
      let estimatedFare = orderPi.estimatedFare
      let estimatedTotalFare = orderPi.estimatedTotalFare
      shouldRecalculateFare <- recalculateFareEnabled
      missingLocationUpdates <-
        (||)
          <$> lastLocUdateTooLongAgo
          <*> thereWereMissingLocUpdates
      if shouldRecalculateFare && not missingLocationUpdates
        then do
          fareParams <- calculateFare transporterId orderPi.vehicleVariant orderPi.traveledDistance orderCase.startTime
          let updatedFare = Fare.fareSum fareParams
              totalFare = Fare.fareSumWithDiscount fareParams
          let distanceDiff = orderPi.traveledDistance - oldDistance
          let priceDiff = updatedFare - estimatedFare
          logTagInfo "Fare recalculation" $
            "Fare difference: "
              <> show (amountToDouble priceDiff)
              <> ", Distance difference: "
              <> show distanceDiff
          putDiffMetric priceDiff distanceDiff
          pure (orderPi.traveledDistance, updatedFare, totalFare)
        else pure (oldDistance, estimatedFare, estimatedTotalFare)
