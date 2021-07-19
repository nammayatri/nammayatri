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
import qualified Types.Storage.SearchRequest as SSearchRequest
import Types.Storage.Organization (Organization)
import qualified Types.Storage.Person as Person
import qualified Types.Storage.ProductInstance as PI
import qualified Types.Storage.Vehicle as Vehicle
import qualified Types.Storage.Ride as Ride
import Utils.Common

data ServiceHandle m = ServiceHandle
  { findPersonById :: Id Person.Person -> m (Maybe Person.Person),
    findPIById :: Id PI.ProductInstance -> m (Maybe PI.ProductInstance),
    findRideById :: Id Ride.Ride -> m (Maybe Ride.Ride),
    endRideTransaction :: Id Ride.Ride -> Id Driver -> Amount -> m (),
    findSearchRequestById :: Id SSearchRequest.SearchRequest -> m (Maybe SSearchRequest.SearchRequest),
    notifyUpdateToBAP :: PI.ProductInstance -> Ride.Ride -> Ride.RideStatus -> m (),
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
  Id Ride.Ride ->
  m APISuccess.APISuccess
endRideHandler ServiceHandle {..} requestorId rideId = do
  requestor <- findPersonById requestorId >>= fromMaybeM PersonNotFound
  ride <- findRideById (cast rideId) >>= fromMaybeM RideDoesNotExist
  driverId <- ride.personId & fromMaybeM (RideFieldNotPresent "person")
  case requestor.role of
    Person.DRIVER -> unless (requestorId == driverId) $ throwError NotAnExecutor
    _ -> throwError AccessDenied
  unless (ride.status == Ride.INPROGRESS) $ throwError $ RideInvalidStatus "This ride cannot be ended"

  let prodInstId = ride.productInstanceId
  prodInst <- findPIById prodInstId >>= fromMaybeM PINotFound
  searchRequest <- findSearchRequestById prodInst.requestId >>= fromMaybeM SearchRequestNotFound
  logTagInfo "endRide" ("DriverId " <> getId requestorId <> ", RideId " <> getId rideId)

  actualPrice <-
    ifM
      recalculateFareEnabled
      (recalculateFare searchRequest ride)
      (ride.price & fromMaybeM (RideFieldNotPresent "price"))

  endRideTransaction ride.id (cast driverId) actualPrice

  notifyUpdateToBAP prodInst (updateActualPrice actualPrice ride){status = Ride.COMPLETED} Ride.COMPLETED

  return APISuccess.Success
  where
    recalculateFare searchRequest orderPi = do
      transporterId <- Id <$> searchRequest.provider & fromMaybeM (SearchRequestFieldNotPresent "provider")
      vehicleVariant <-
        (searchRequest.udf1 >>= readMaybe . T.unpack)
          & fromMaybeM (SearchRequestFieldNotPresent "udf1")
      oldDistance <-
        (searchRequest.udf5 >>= readMaybe . T.unpack)
          & fromMaybeM (SearchRequestFieldNotPresent "udf5")
      fare <- calculateFare transporterId vehicleVariant orderPi.distance searchRequest.startTime
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
    updateActualPrice :: Amount -> Ride.Ride -> Ride.Ride
    updateActualPrice = \p ride -> ride{actualPrice = Just p}
