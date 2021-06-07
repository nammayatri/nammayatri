module Product.Ride
  ( setDriverAcceptance,
  )
where

import App.Types
import qualified Beckn.Storage.Redis.Queries as Redis
import Beckn.Types.APISuccess
import Beckn.Types.Common hiding (id)
import qualified Beckn.Types.Storage.RegistrationToken as SR
import EulerHS.Prelude hiding (id)
import Types.API.Ride
import Utils.Common

setDriverAcceptance :: SR.RegistrationToken -> SetDriverAcceptanceReq -> FlowHandler SetDriverAcceptanceRes
setDriverAcceptance SR.RegistrationToken {..} SetDriverAcceptanceReq {..} = withFlowHandlerAPI $ do
  now <- getCurrentTime
  logTagInfo "setDriverAcceptance" $ redisKey <> " " <> show response
  Redis.setExRedis redisKey (driverResponse now) 600
  pure Success
  where
    driverId = entityId
    productInstanceId_ = productInstanceId.getId
    redisKey = "beckn:" <> productInstanceId_ <> ":" <> driverId <> ":response"
    driverResponse now = DriverResponse {status = response, respondedAt = now}
