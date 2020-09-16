module MockProviderBackend.Fixtures where

import qualified "mock-provider-backend" App as MockProviderBE
import Beckn.Types.Common as Common
import Beckn.Types.Core.Context
import Beckn.Types.Core.Domain
import Beckn.Types.Core.Quotation
import Beckn.Types.FMD.API.Confirm
import Beckn.Types.FMD.API.Init
import Beckn.Types.FMD.API.Search
import Beckn.Types.FMD.API.Select
import Beckn.Types.FMD.API.Update
import Beckn.Types.FMD.Order
import Beckn.Utils.Common
import Data.Time
import EulerHS.Prelude
import Servant.Client

buildContext :: Text -> Text -> UTCTime -> Context
buildContext act tid utcTime =
  Context
    { _domain = FINAL_MILE_DELIVERY,
      _action = act,
      _country = Nothing,
      _city = Nothing,
      _core_version = Just "0.8.0",
      _domain_version = Just "0.8.2",
      _bap_uri = Nothing,
      _bpp_uri = Nothing,
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
searchFlow = client searchAPI

buildFMDSearchReq :: Context -> SearchReq
buildFMDSearchReq context =
  SearchReq
    { context,
      message = SearchIntent example
    }

selectFlow :: Text -> SelectReq -> ClientM Common.AckResponse
selectFlow = client selectAPI

buildFMDSelectReq :: Context -> SelectReq
buildFMDSelectReq context =
  SelectReq
    { context,
      message = SelectOrder example
    }

initFlow :: Text -> InitReq -> ClientM Common.AckResponse
initFlow = client initAPI

buildFMDInitReq :: Context -> Text -> InitReq
buildFMDInitReq context quoteId = do
  let order = example
  InitReq
    { context,
      message = InitOrder $ order {_quotation = Just (Quotation quoteId Nothing Nothing Nothing)}
    }

confirmFlow :: Text -> ConfirmReq -> ClientM Common.AckResponse
confirmFlow = client confirmAPI

buildFMDConfirmReq :: Context -> ConfirmReq
buildFMDConfirmReq context =
  ConfirmReq
    { context,
      message = ConfirmReqMessage example
    }

updateFlow :: Text -> UpdateReq -> ClientM Common.AckResponse
updateFlow = client updateAPI

buildFMDUpdateReq :: Context -> UpdateReq
buildFMDUpdateReq context =
  UpdateReq
    { context,
      message = UpdateReqMessage "update_pickup_location" example
    }

mockProviderApiKey :: Text
mockProviderApiKey = "test-provider-2-key"
