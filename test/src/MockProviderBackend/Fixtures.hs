module MockProviderBackend.Fixtures where

import qualified "mock-provider-backend" App as MockProviderBE
import "mock-provider-backend" App.Handlers as MockProviderRoutes
import Beckn.Types.Common as Common
import Beckn.Types.Core.Context
import Beckn.Types.FMD.API.Confirm
import Beckn.Types.FMD.API.Init
import Beckn.Types.FMD.API.Search
import Beckn.Types.FMD.API.Select
import Beckn.Utils.Common
import Data.Time
import EulerHS.Prelude
import Servant.Client

buildContext :: Text -> Text -> UTCTime -> Context
buildContext act tid utcTime =
  Context
    { _domain = "FINAL-MILE-DELIVERY",
      _action = act,
      _country = Nothing,
      _city = Nothing,
      _core_version = Just "0.8.0",
      _domain_version = Just "0.7.0",
      _ac_id = Nothing,
      _transaction_id = tid,
      _message_id = tid,
      _timestamp = utcTime
    }

mockProviderBaseUrl :: BaseUrl
mockProviderBaseUrl =
  BaseUrl
    { baseUrlScheme = Http,
      baseUrlHost = "localhost",
      baseUrlPort = 8017,
      baseUrlPath = "/v1"
    }

startServer :: IO ThreadId
startServer = forkIO MockProviderBE.runMockProvider

searchFlow :: Text -> SearchReq -> ClientM Common.AckResponse
searchFlow = client (Proxy :: Proxy MockProviderRoutes.ProviderSearchAPI)

buildFMDSearchReq :: Context -> SearchReq
buildFMDSearchReq context =
  SearchReq
    { context,
      message = SearchIntent example
    }

selectFlow :: Text -> SelectReq -> ClientM Common.AckResponse
selectFlow = client (Proxy :: Proxy MockProviderRoutes.ProviderSelectAPI)

buildFMDSelectReq :: Context -> SelectReq
buildFMDSelectReq context =
  SelectReq
    { context,
      message = DraftOrder example
    }

initFlow :: Text -> InitReq -> ClientM Common.AckResponse
initFlow = client (Proxy :: Proxy MockProviderRoutes.ProviderInitAPI)

buildFMDInitReq :: Context -> Text -> InitReq
buildFMDInitReq context quoteId =
  InitReq
    { context,
      message = InitReqMessage quoteId example
    }

confirmFlow :: Text -> ConfirmReq -> ClientM Common.AckResponse
confirmFlow = client (Proxy :: Proxy MockProviderRoutes.ProviderConfirmAPI)

buildFMDConfirmReq :: Context -> ConfirmReq
buildFMDConfirmReq context =
  ConfirmReq
    { context,
      message = ConfirmReqMessage example
    }
