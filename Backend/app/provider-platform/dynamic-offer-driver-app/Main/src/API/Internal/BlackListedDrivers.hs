module API.Internal.BlackListedDrivers where

import qualified Domain.Action.Internal.BlackListedDrivers as Domain
import Domain.Types.Merchant (Merchant)
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Ride as Ride
import Environment
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant

type API =
  Capture "merchantId" (Id Merchant)
    :> Capture "rideId" (Id Ride.Ride)
    :> "blackListDriver"
    :> Header "token" Text
    :> ReqBody '[JSON] Domain.BlackListDriverReq
    :> Post '[JSON] APISuccess
    :<|> Capture "merchantId" (Id Merchant)
      :> "getBlackListedDrivers"
      :> Header "token" Text
      :> ReqBody '[JSON] Domain.GetBlackListedDriverInfoReq
      :> Get '[JSON] [Domain.BlackListedDriverResp]
    :<|> Capture "merchantId" (Id Merchant)
      :> Capture "driverId" (Id Person.Person)
      :> "unblockDriver"
      :> Header "token" Text
      :> ReqBody '[JSON] Domain.GetBlackListedDriverInfoReq
      :> Post '[JSON] APISuccess

handler :: FlowServer API
handler = blackListDriver :<|> getBlackListedDrivers :<|> unblockDriver

blackListDriver :: Id Merchant -> Id Ride.Ride -> Maybe Text -> Domain.BlackListDriverReq -> FlowHandler APISuccess
blackListDriver merchantId rideId apiKey = withFlowHandlerAPI . Domain.blackListDriver merchantId rideId apiKey

getBlackListedDrivers :: Id Merchant -> Maybe Text -> Domain.GetBlackListedDriverInfoReq -> FlowHandler [Domain.BlackListedDriverResp]
getBlackListedDrivers merchantId apiKey = withFlowHandlerAPI . Domain.getBlackListedDrivers merchantId apiKey

unblockDriver :: Id Merchant -> Id Person.Person -> Maybe Text -> Domain.GetBlackListedDriverInfoReq -> FlowHandler APISuccess
unblockDriver merchantId driverId apiKey = withFlowHandlerAPI . Domain.unblockDriver merchantId driverId apiKey
