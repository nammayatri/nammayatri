module App.Routes where

import App.Types
import Beckn.Types.Common (AckResponse (..))
import Beckn.Types.FMD.API.Confirm (OnConfirmReq)
import Beckn.Types.FMD.API.Init (OnInitReq)
import Beckn.Types.FMD.API.Search (OnSearchReq)
import Beckn.Types.FMD.API.Select (OnSelectReq)
import Beckn.Utils.Servant.HeaderAuth
import qualified Data.Vault.Lazy as V
import EulerHS.Prelude
import qualified Product.Confirm as P
import qualified Product.Init as P
import qualified Product.Search as P
import qualified Product.Select as P
import qualified Product.Trigger as T
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

type TriggerAPI =
  "trigger"
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
