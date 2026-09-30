module API.Internal.BlackListedDrivers where

import qualified Domain.Action.Internal.BlackListedDrivers as Domain
import Domain.Types.Merchant (Merchant)
import qualified Domain.Types.Person as Person
import Environment
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant

type API =
  Capture "merchantId" (Id Merchant)
    :> Capture "driverId" (Id Person.Person)
    :> "blackListDriver"
    :> Header "token" Text
    :> ReqBody '[JSON] Domain.BlackListDriverReq
    :> Post '[JSON] APISuccess

handler :: FlowServer API
handler = blackListDriver

blackListDriver :: Id Merchant -> Id Person.Person -> Maybe Text -> Domain.BlackListDriverReq -> FlowHandler APISuccess
blackListDriver merchantId driverId apiKey = withFlowHandlerAPI . Domain.blackListDriver merchantId driverId apiKey
