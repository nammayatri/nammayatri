{-# LANGUAGE OverloadedLabels #-}

module Product.Ride
  ( setDriverAcceptance,
  )
where

import App.Types (FlowHandler)
import qualified Beckn.Storage.Redis.Queries as Redis
import Beckn.Types.Common
import Beckn.Types.Core.Ack (Ack (..), Status (..))
import qualified Beckn.Types.Storage.RegistrationToken as SR
import Beckn.Utils.Common
import EulerHS.Prelude
import Types.API.Ride

setDriverAcceptance :: SR.RegistrationToken -> SetDriverAcceptanceReq -> FlowHandler SetDriverAcceptanceRes
setDriverAcceptance SR.RegistrationToken {..} SetDriverAcceptanceReq {..} = withFlowHandler $ do
  now <- getCurrentTime
  Redis.setExRedis redisKey (driverResponse now) 600
  pure $ Ack {_status = ACK}
  where
    driverId = _EntityId
    productInstanceId = _productInstanceId ^. #getId
    redisKey = "beckn:" <> productInstanceId <> ":" <> driverId <> ":response"
    driverResponse now = DriverResponse {_status = _response, _respondedAt = now}
