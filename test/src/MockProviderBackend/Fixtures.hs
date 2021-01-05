module MockProviderBackend.Fixtures where

import Beckn.Types.Core.Ack
import Beckn.Types.FMD.API.Confirm
import Beckn.Types.FMD.API.Init
import Beckn.Types.FMD.API.Search
import Beckn.Types.FMD.API.Select
import Beckn.Types.FMD.API.Update
import EulerHS.Prelude
import Servant
import Servant.Client

mockProviderBaseUrl :: BaseUrl
mockProviderBaseUrl =
  BaseUrl
    { baseUrlScheme = Http,
      baseUrlHost = "localhost",
      baseUrlPort = 8017,
      baseUrlPath = "/v1"
    }

searchFlow :: Maybe Text -> SearchReq -> ClientM AckResponse
searchFlow = client (Proxy :: Proxy (Header "Authorization" Text :> SearchAPI))

selectFlow :: Maybe Text -> SelectReq -> ClientM AckResponse
selectFlow = client (Proxy :: Proxy (Header "Authorization" Text :> SelectAPI))

initFlow :: Maybe Text -> InitReq -> ClientM AckResponse
initFlow = client (Proxy :: Proxy (Header "Authorization" Text :> InitAPI))

confirmFlow :: Maybe Text -> ConfirmReq -> ClientM AckResponse
confirmFlow = client (Proxy :: Proxy (Header "Authorization" Text :> ConfirmAPI))

updateFlow :: Maybe Text -> UpdateReq -> ClientM AckResponse
updateFlow = client (Proxy :: Proxy (Header "Authorization" Text :> UpdateAPI))

mockProviderApiKey :: Text
mockProviderApiKey = "juspay-mock-bpp-1-key"

mockProviderSelfId :: Text
mockProviderSelfId = "JUSPAY.BPP.MOCK.1"
