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
import Storage.Beam.SystemConfigs ()

type API =
  "auth"
    :> Header "token" RegToken
    :> Header "api-key" Text
    :> Get '[JSON] Domain.InternalResp

handler :: FlowServer API
handler =
  internalAuth

internalAuth :: Maybe RegToken -> Maybe Text -> FlowHandler Domain.InternalResp
internalAuth token apiKey = withFlowHandlerAPI $ Domain.internalAuth token apiKey
