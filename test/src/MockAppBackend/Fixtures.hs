module MockAppBackend.Fixtures where

import "mock-app-backend" App.Routes as MockAppRoutes
import Beckn.Types.Core.API.Callback
import Beckn.Types.Core.Ack
import Beckn.Types.Core.Context
import Beckn.Types.FMD.API.Confirm
import Beckn.Types.FMD.API.Init
import Beckn.Types.FMD.API.Search
import Beckn.Types.FMD.API.Select
import Beckn.Types.FMD.API.Update
import Beckn.Utils.Common
import EulerHS.Prelude
import qualified "mock-app-backend" Product.Trigger as MockAppTrigger
import Servant.API
import Servant.Client

mockAppBaseUrl :: BaseUrl
mockAppBaseUrl =
  BaseUrl
    { baseUrlScheme = Http,
      baseUrlHost = "localhost",
      baseUrlPort = 8016,
      baseUrlPath = "/v1"
    }

triggerSearchReq :: MockAppTrigger.TriggerFlow -> ClientM AckResponse
triggerTrack :: Text -> ClientM AckResponse
triggerTrackForLast :: ClientM AckResponse
triggerCancel :: Text -> ClientM AckResponse
triggerCancelLast :: ClientM AckResponse
triggerUpdate :: Text -> MockAppTrigger.TriggerUpdateMode -> ClientM AckResponse
triggerUpdateLast :: MockAppTrigger.TriggerUpdateMode -> ClientM AckResponse
triggerSearchReq
  :<|> _
  :<|> triggerTrackForLast
  :<|> triggerTrack
  :<|> triggerCancelLast
  :<|> triggerCancel
  :<|> triggerUpdateLast
  :<|> triggerUpdate = client (Proxy :: Proxy MockAppRoutes.TriggerAPI)

onSearchFlow :: Text -> OnSearchReq -> ClientM AckResponse
onSearchFlow = client (Proxy :: Proxy MockAppRoutes.OnSearchAPI)

buildOnSearchReq :: Context -> OnSearchReq
buildOnSearchReq context =
  CallbackReq
    { context,
      contents = Right $ OnSearchServices example
    }

onSelectFlow :: Text -> OnSelectReq -> ClientM AckResponse
onSelectFlow = client (Proxy :: Proxy MockAppRoutes.OnSelectAPI)

buildOnSelectReq :: Context -> OnSelectReq
buildOnSelectReq context =
  CallbackReq
    { context,
      contents = Right $ SelectOrder example
    }

onInitFlow :: Text -> OnInitReq -> ClientM AckResponse
onInitFlow = client (Proxy :: Proxy MockAppRoutes.OnInitAPI)

buildOnInitReq :: Context -> OnInitReq
buildOnInitReq context =
  CallbackReq
    { context,
      contents = Right $ InitOrder example
    }

onConfirmFlow :: Text -> OnConfirmReq -> ClientM AckResponse
onConfirmFlow = client (Proxy :: Proxy MockAppRoutes.OnConfirmAPI)

buildOnConfirmReq :: Context -> OnConfirmReq
buildOnConfirmReq context =
  CallbackReq
    { context,
      contents = Right $ ConfirmResMessage example
    }

onUpdateFlow :: Text -> OnUpdateReq -> ClientM AckResponse
onUpdateFlow = client (Proxy :: Proxy MockAppRoutes.OnUpdateAPI)

buildOnUpdateReq :: Context -> OnUpdateReq
buildOnUpdateReq context =
  CallbackReq
    { context,
      contents = Right $ UpdateResMessage example
    }

mockAppApiKey :: Text
mockAppApiKey = "test-app-2-key"
