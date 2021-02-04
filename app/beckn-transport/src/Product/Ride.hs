{-# LANGUAGE OverloadedLabels #-}

module Product.Ride
  ( setDriverAcceptance,
  )
where

import App.Types (FlowHandler)
import qualified Beckn.Storage.Redis.Queries as Redis
import qualified Beckn.Types.APIResult as APIResult
import qualified Beckn.Types.Storage.RegistrationToken as SR
import Beckn.Utils.Common
import EulerHS.Prelude
import Types.API.Ride
import Types.App

setDriverAcceptance :: SR.RegistrationToken -> SetDriverAcceptanceReq -> FlowHandler SetDriverAcceptanceRes
setDriverAcceptance SR.RegistrationToken {..} SetDriverAcceptanceReq {..} = withFlowHandler $ do
  now <- getCurrTime
  Redis.setExRedis redisKey (redisValue now) 600
  return APIResult.Success
  where
    driverId = DriverId _EntityId
    redisKey = "beckn:" <> _productInstanceId ^. #_getProductInstanceId <> ":response"
    redisValue now = DriverResponse {_driverId = driverId, _productInstanceId = _productInstanceId, _status = _response, _respondedAt = now}