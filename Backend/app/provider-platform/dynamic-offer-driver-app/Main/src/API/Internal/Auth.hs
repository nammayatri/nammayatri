module API.Internal.Auth
  ( API,
    handler,
  )
where

import qualified Domain.Action.Internal.Auth as Domain
import Environment
import EulerHS.Prelude
import Kernel.Utils.Common
import Servant

type API =
  "auth"
    :> Header "token" Text
    :> Header "api-key" Text
    :> Header "merchant-id" Text
    :> Get '[JSON] Domain.InternalResp

handler :: FlowServer API
handler =
  internalAuth

internalAuth :: Maybe Text -> Maybe Text -> Maybe Text -> FlowHandler Domain.InternalResp
internalAuth token apiKey merchantId = withFlowHandlerAPI $ Domain.internalAuth token apiKey merchantId
