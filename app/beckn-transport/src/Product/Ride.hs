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

setDriverAcceptance :: SR.RegistrationToken -> SetDriverAcceptanceReq -> FlowHandler SetDriverAcceptanceRes
setDriverAcceptance SR.RegistrationToken {..} SetDriverAcceptanceReq {..} = withFlowHandler $ do
  now <- getCurrTime
  Redis.setExRedis redisKey (driverResponse now) 600
  return APIResult.Success
  where
    productInstanceId = _productInstanceId ^. #_getProductInstanceId
    redisKey = "beckn:" <> productInstanceId <> ":" <> _EntityId <> ":response"
    driverResponse now = DriverResponse {_status = _response, _respondedAt = now}