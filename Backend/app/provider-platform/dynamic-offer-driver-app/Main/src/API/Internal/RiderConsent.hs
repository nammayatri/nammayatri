module API.Internal.RiderConsent where

import qualified Domain.Action.Internal.RiderConsent as Domain
import Domain.Types.Merchant (Merchant)
import Environment
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant

type API =
  Capture "merchantId" (Id Merchant)
    :> "riderDetails"
    :> "consent"
    :> Header "token" Text
    :> ReqBody '[JSON] Domain.SetRiderConsentReq
    :> Post '[JSON] Domain.SetRiderConsentRes

handler :: FlowServer API
handler = setRiderConsent

setRiderConsent :: Id Merchant -> Maybe Text -> Domain.SetRiderConsentReq -> FlowHandler Domain.SetRiderConsentRes
setRiderConsent merchantId apiKey = withFlowHandlerAPI . Domain.setRiderConsent merchantId apiKey
