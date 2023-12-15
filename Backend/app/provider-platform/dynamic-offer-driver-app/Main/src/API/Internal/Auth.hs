module API.Internal.Auth
  ( API,
    handler,
  )
where

import qualified Domain.Action.Internal.Auth as Domain
import Domain.Types.Merchant
import Environment
import EulerHS.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()

type API =
  "auth"
    :> Header "token" RegToken
    :> Header "api-key" Text
    :> Header "merchant-id" (Id Merchant)
    :> Get '[JSON] Domain.InternalResp

handler :: FlowServer API
handler =
  internalAuth

internalAuth :: Maybe RegToken -> Maybe Text -> Maybe (Id Merchant) -> FlowHandler Domain.InternalResp
internalAuth token apiKey merchantId = withFlowHandlerAPI $ Domain.internalAuth token apiKey merchantId
