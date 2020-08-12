module App.Routes where

import App.Types
import Beckn.Types.Common (AckResponse (..))
import Beckn.Types.FMD.API.Cancel (OnCancelReq)
import Beckn.Types.FMD.API.Confirm (OnConfirmReq)
import Beckn.Types.FMD.API.Init (OnInitReq)
import Beckn.Types.FMD.API.Search (OnSearchReq)
import Beckn.Types.FMD.API.Select (OnSelectReq)
import Beckn.Types.FMD.API.Status (OnStatusReq)
import Beckn.Types.FMD.API.Track (OnTrackReq)
import Beckn.Types.FMD.API.Update (OnUpdateReq)
import Beckn.Utils.Servant.HeaderAuth
import qualified Data.Vault.Lazy as V
import EulerHS.Prelude
import qualified Product.Cancel as P
import qualified Product.Confirm as P
import qualified Product.Init as P
import qualified Product.Search as P
import qualified Product.Select as P
import qualified Product.Status as P
import qualified Product.Track as P
import qualified Product.Trigger as T
import qualified Product.Update as P
import Servant hiding (Context)
import Utils.Auth

type MockAppBackendAPI =
  "v1"
    :> ( Get '[JSON] Text
           :<|> TriggerAPI
           :<|> OnSearchAPI
           :<|> OnSelectAPI
           :<|> OnInitAPI
           :<|> OnConfirmAPI
           :<|> OnTrackAPI
           :<|> OnStatusAPI
           :<|> OnCancelAPI
           :<|> OnUpdateAPI
       )

mockAppBackendAPI :: Proxy MockAppBackendAPI
mockAppBackendAPI = Proxy

mockAppBackendServer :: V.Key (HashMap Text Text) -> FlowServer MockAppBackendAPI
mockAppBackendServer _key =
  pure "Mock app backend is UP"
    :<|> triggerFlow
    :<|> onSearchFlow
    :<|> onSelectFlow
    :<|> onInitFlow
    :<|> onConfirmFlow
    :<|> onTrackFlow
    :<|> onStatusFlow
    :<|> onCancelFlow
    :<|> onUpdateFlow

type TriggerAPI =
  "trigger"
    :> QueryParam' '[Required, String] "flow" T.TriggerFlow
    :> Get '[JSON] AckResponse

triggerFlow :: FlowServer TriggerAPI
triggerFlow = T.trigger

type OnSearchAPI =
  "on_search"
    :> APIKeyAuth VerifyAPIKey
    :> ReqBody '[JSON] OnSearchReq
    :> Post '[JSON] AckResponse

onSearchFlow :: FlowServer OnSearchAPI
onSearchFlow = P.searchCb

type OnSelectAPI =
  "on_select"
    :> APIKeyAuth VerifyAPIKey
    :> ReqBody '[JSON] OnSelectReq
    :> Post '[JSON] AckResponse

onSelectFlow :: FlowServer OnSelectAPI
onSelectFlow = P.selectCb

type OnInitAPI =
  "on_init"
    :> APIKeyAuth VerifyAPIKey
    :> ReqBody '[JSON] OnInitReq
    :> Post '[JSON] AckResponse

onInitFlow :: FlowServer OnInitAPI
onInitFlow = P.initCb

type OnConfirmAPI =
  "on_confirm"
    :> APIKeyAuth VerifyAPIKey
    :> ReqBody '[JSON] OnConfirmReq
    :> Post '[JSON] AckResponse

onConfirmFlow :: FlowServer OnConfirmAPI
onConfirmFlow = P.confirmCb

type OnTrackAPI =
  "on_track"
    :> APIKeyAuth VerifyAPIKey
    :> ReqBody '[JSON] OnTrackReq
    :> Post '[JSON] AckResponse

onTrackFlow :: FlowServer OnTrackAPI
onTrackFlow = P.trackCb

type OnStatusAPI =
  "on_status"
    :> APIKeyAuth VerifyAPIKey
    :> ReqBody '[JSON] OnStatusReq
    :> Post '[JSON] AckResponse

onStatusFlow :: FlowServer OnStatusAPI
onStatusFlow = P.statusCb

type OnCancelAPI =
  "on_cancel"
    :> APIKeyAuth VerifyAPIKey
    :> ReqBody '[JSON] OnCancelReq
    :> Post '[JSON] AckResponse

onCancelFlow :: FlowServer OnCancelAPI
onCancelFlow = P.cancelCb

type OnUpdateAPI =
  "on_update"
    :> APIKeyAuth VerifyAPIKey
    :> ReqBody '[JSON] OnUpdateReq
    :> Post '[JSON] AckResponse

onUpdateFlow :: FlowServer OnUpdateAPI
onUpdateFlow = P.updateCb
