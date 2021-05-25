{-# LANGUAGE OverloadedLabels #-}

module Product.Ride
  ( setDriverAcceptance,
  )
where

import App.Types
import qualified Beckn.Storage.Redis.Queries as Redis
import Beckn.Types.APISuccess
import Beckn.Types.Common
import qualified Beckn.Types.Storage.RegistrationToken as SR
import EulerHS.Prelude
import Types.API.Ride
import Utils.Common

setDriverAcceptance :: SR.RegistrationToken -> SetDriverAcceptanceReq -> FlowHandler SetDriverAcceptanceRes
setDriverAcceptance SR.RegistrationToken {..} SetDriverAcceptanceReq {..} = withFlowHandlerAPI $ do
  now <- getCurrentTime
  Redis.setExRedis redisKey (driverResponse now) 600
  pure Success
  where
    driverId = _EntityId
    productInstanceId = _productInstanceId ^. #getId
    redisKey = "beckn:" <> productInstanceId <> ":" <> driverId <> ":response"
    driverResponse now = DriverResponse {_status = _response, _respondedAt = now}
