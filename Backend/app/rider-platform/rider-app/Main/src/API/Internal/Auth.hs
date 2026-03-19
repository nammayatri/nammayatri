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
    :> ( Header "token" RegToken
           :> Header "api-key" Text
           :> Get '[JSON] Domain.InternalResp
           :<|> "getToken"
             :> Header "token" Text
             :> QueryParam "mobileNumber" Text
             :> QueryParam "mobileCountryCode" Text
             :> Get '[JSON] Domain.CustomerAuthTokenResp
       )

handler :: FlowServer API
handler =
  internalAuth
    :<|> getToken

internalAuth :: Maybe RegToken -> Maybe Text -> FlowHandler Domain.InternalResp
internalAuth token apiKey = withFlowHandlerAPI $ Domain.internalAuth token apiKey

getToken :: Maybe Text -> Maybe Text -> Maybe Text -> FlowHandler Domain.CustomerAuthTokenResp
getToken apiKey mobileNumber mobileCountryCode =
  withFlowHandlerAPI $ Domain.getCustomerAuthToken apiKey mobileNumber mobileCountryCode
