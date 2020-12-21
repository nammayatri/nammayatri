module MockProviderBackend.Fixtures where

import Beckn.Types.Core.Ack
import Beckn.Types.FMD.API.Confirm
import Beckn.Types.FMD.API.Init
import Beckn.Types.FMD.API.Search
import Beckn.Types.FMD.API.Select
import Beckn.Types.FMD.API.Update
import EulerHS.Prelude
import Servant ((:<|>) (..))
import Servant.Client

mockProviderBaseUrl :: BaseUrl
mockProviderBaseUrl =
  BaseUrl
    { baseUrlScheme = Http,
      baseUrlHost = "localhost",
      baseUrlPort = 8017,
      baseUrlPath = "/v1"
    }

searchFlow :: Text -> SearchReq -> ClientM AckResponse
_ :<|> searchFlow = client searchAPI

selectFlow :: Text -> SelectReq -> ClientM AckResponse
_ :<|> selectFlow = client selectAPI

initFlow :: Text -> InitReq -> ClientM AckResponse
_ :<|> initFlow = client initAPI

confirmFlow :: Text -> ConfirmReq -> ClientM AckResponse
_ :<|> confirmFlow = client confirmAPI

updateFlow :: Text -> UpdateReq -> ClientM AckResponse
_ :<|> updateFlow = client updateAPI

mockProviderApiKey :: Text
mockProviderApiKey = "test-provider-2-key"
