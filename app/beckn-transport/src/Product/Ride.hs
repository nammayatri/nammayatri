{-# LANGUAGE OverloadedLabels #-}

module Product.Ride
  ( setDriverAcceptance,
  )
where

import App.Types (FlowHandler)
import qualified Beckn.Storage.Redis.Queries as Redis
import qualified Beckn.Types.APIResult as APIResult
import qualified Beckn.Types.Storage.RegistrationToken as SR
import Beckn.Utils.Common (withFlowHandler)
import EulerHS.Prelude
import Types.API.Ride

setDriverAcceptance :: SR.RegistrationToken -> SetDriverAcceptanceReq -> FlowHandler SetDriverAcceptanceRes
setDriverAcceptance _ SetDriverAcceptanceReq {..} = withFlowHandler $ do
  Redis.setExRedis redisKey _response 600
  return APIResult.Success
  where
    redisKey = "beckn:" <> _productInstanceId ^. #_getProductInstanceId <> ":response"