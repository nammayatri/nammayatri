module MockAppBackend.Fixtures where

import qualified "mock-app-backend" App as MockAppBE
import "mock-app-backend" App.Routes as MockAppRoutes
import Beckn.Types.Common as Common
import Beckn.Types.Core.Context
import Beckn.Types.FMD.API.Callback
import Beckn.Types.FMD.API.Confirm
import Beckn.Types.FMD.API.Init
import Beckn.Types.FMD.API.Search
import Beckn.Types.FMD.API.Select
import Beckn.Utils.Common
import Data.Time
import EulerHS.Prelude
import qualified "mock-app-backend" Product.Trigger as MockAppTrigger
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

mockAppBaseUrl :: BaseUrl
mockAppBaseUrl =
  BaseUrl
    { baseUrlScheme = Http,
      baseUrlHost = "localhost",
      baseUrlPort = 8016,
      baseUrlPath = "/v1"
    }

startServer :: IO ThreadId
startServer = forkIO MockAppBE.runMockApp

triggerSearchReq :: MockAppTrigger.TriggerFlow -> ClientM Common.AckResponse
triggerSearchReq = client (Proxy :: Proxy MockAppRoutes.TriggerAPI)

onSearchFlow :: Text -> OnSearchReq -> ClientM Common.AckResponse
onSearchFlow = client (Proxy :: Proxy MockAppRoutes.OnSearchAPI)

buildOnSearchReq :: Context -> OnSearchReq
buildOnSearchReq context =
  CallbackReq
    { context,
      contents = Right $ OnSearchServices example
    }

onSelectFlow :: Text -> OnSelectReq -> ClientM Common.AckResponse
onSelectFlow = client (Proxy :: Proxy MockAppRoutes.OnSelectAPI)

buildOnSelectReq :: Context -> OnSelectReq
buildOnSelectReq context =
  CallbackReq
    { context,
      contents = Right $ OnSelectMessage example example
    }

onInitFlow :: Text -> OnInitReq -> ClientM Common.AckResponse
onInitFlow = client (Proxy :: Proxy MockAppRoutes.OnInitAPI)

buildOnInitReq :: Context -> OnInitReq
buildOnInitReq context =
  CallbackReq
    { context,
      contents = Right $ InitResMessage example
    }

onConfirmFlow :: Text -> OnConfirmReq -> ClientM Common.AckResponse
onConfirmFlow = client (Proxy :: Proxy MockAppRoutes.OnConfirmAPI)

buildOnConfirmReq :: Context -> OnConfirmReq
buildOnConfirmReq context =
  CallbackReq
    { context,
      contents = Right $ ConfirmResMessage example
    }
