module Product.Ride
  ( setDriverAcceptance,
  )
where

import App.Types
import qualified Beckn.Storage.Redis.Queries as Redis
import Beckn.Types.APISuccess
import Beckn.Types.Common hiding (id)
import Beckn.Types.Id (Id (..), cast)
import qualified Beckn.Types.Storage.RegistrationToken as SR
import EulerHS.Prelude hiding (id)
import qualified Storage.Queries.AllocationEvent as AllocationEvent
import Types.API.Ride
import qualified Types.Storage.AllocationEvent as AllocationEvent
import Utils.Common

setDriverAcceptance :: SR.RegistrationToken -> SetDriverAcceptanceReq -> FlowHandler SetDriverAcceptanceRes
setDriverAcceptance SR.RegistrationToken {..} SetDriverAcceptanceReq {..} = withFlowHandlerAPI $ do
  now <- getCurrentTime
  logTagInfo "setDriverAcceptance" $ redisKey <> " " <> show response
  Redis.setExRedis redisKey (driverResponse now) 600
  AllocationEvent.logAllocationEvent (getEventType response) (cast productInstanceId) (Just $ Id driverId)
  pure Success
  where
    driverId = entityId
    productInstanceId_ = productInstanceId.getId
    redisKey = "beckn:" <> productInstanceId_ <> ":" <> driverId <> ":response"
    driverResponse now = DriverResponse {status = response, respondedAt = now}
    getEventType ACCEPT = AllocationEvent.AcceptedByDriver
    getEventType REJECT = AllocationEvent.RejectedByDriver
