{-# LANGUAGE OverloadedLabels #-}

module Product.Ride
  ( setDriverAcceptance,
    cancelRide,
  )
where

import App.Types (FlowHandler)
import qualified Beckn.Storage.Redis.Queries as Redis
import qualified Beckn.Types.APIResult as APIResult
import Beckn.Types.App (PersonId (..), ProductInstanceId (..))
import Beckn.Types.Core.Ack (Ack (..))
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Person as Person
import qualified Beckn.Types.Storage.RegistrationToken as SR
import Beckn.Utils.Common
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.ProductInstance as QProductInstance
import Storage.Queries.RideRequest as RideRequest
import Types.API.Ride
import Types.App (RideId (..), RideRequestId (..))
import Types.Storage.RideRequest as SRideRequest

setDriverAcceptance :: SR.RegistrationToken -> SetDriverAcceptanceReq -> FlowHandler SetDriverAcceptanceRes
setDriverAcceptance SR.RegistrationToken {..} SetDriverAcceptanceReq {..} = withFlowHandler $ do
  now <- getCurrTime
  Redis.setExRedis redisKey (driverResponse now) 600
  pure $ Ack {_status = "ACK"}
  where
    driverId = _EntityId
    productInstanceId = _productInstanceId ^. #getId
    redisKey = "beckn:" <> productInstanceId <> ":" <> driverId <> ":response"
    driverResponse now = DriverResponse {_status = _response, _respondedAt = now}

cancelRide :: SR.RegistrationToken -> Text -> FlowHandler APIResult.APIResult
cancelRide SR.RegistrationToken {..} rideId = withFlowHandler $ do
  prodInst <- QProductInstance.findById $ ProductInstanceId rideId
  unless (prodInst ^. #_type == Case.RIDEORDER) $ throwError400 "WRONG_PRODUCT_INSTANCE_TYPE"
  driverId <- fromMaybeM400 "ORDER_WITHOUT_DRIVER" (prodInst ^. #_personId)
  authPerson <- QPerson.findPersonById $ PersonId _EntityId
  if adminOrRideDriver authPerson driverId
    then RideRequest.create =<< mkRideReq
    else throwError403 "NOT_AN_ORDER_EXECUTER"
  pure APIResult.Success
  where
    adminOrRideDriver authPerson driverId =
      authPerson ^. #_role == Person.ADMIN
        || authPerson ^. #_role == Person.DRIVER && authPerson ^. #_id == driverId
    mkRideReq = do
      guid <- L.generateGUID
      let rId = RideId rideId
      currTime <- getCurrTime
      pure
        SRideRequest.RideRequest
          { _id = RideRequestId guid,
            _rideId = rId,
            _createdAt = currTime,
            _lastProcessTime = currTime,
            _type = SRideRequest.CANCELLATION
          }
