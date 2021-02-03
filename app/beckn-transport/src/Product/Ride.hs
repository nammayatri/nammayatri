module Product.Ride
  ( setDriverAcceptance,
  )
where

import App.Types (FlowHandler)
import qualified Beckn.Storage.Redis.Queries as Redis
import qualified Beckn.Types.APIResult as APIResult
import Beckn.Types.App ( ProductInstanceId(ProductInstanceId) ) 
import qualified Beckn.Types.Storage.RegistrationToken as SR
import Beckn.Utils.Common (withFlowHandler)
import EulerHS.Prelude 
import Types.API.Ride (NotificationStatus)

setDriverAcceptance :: SR.RegistrationToken -> ProductInstanceId -> NotificationStatus -> FlowHandler APIResult.APIResult
setDriverAcceptance _ (ProductInstanceId productInstanceId) response = withFlowHandler $ do
  Redis.setKeyRedis redisKey response
  return APIResult.Success
  where
    redisKey = "beckn:"<> productInstanceId <> ":response"