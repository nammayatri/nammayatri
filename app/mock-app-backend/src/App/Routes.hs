module App.Routes where

import App.Types
import Beckn.Types.Common (AckResponse (..))
import Beckn.Utils.Servant.HeaderAuth
import qualified Data.Vault.Lazy as V
import EulerHS.Prelude
import qualified Product.Search as P
import Servant hiding (Context)
import "beckn-gateway" Types.API.Search (OnSearchReq)
import Utils.Auth

type MockAppBackendAPI =
  "v1"
    :> ( Get '[JSON] Text
           :<|> TriggerSearchAPI
           :<|> OnSearchAPI
       )

mockAppBackendAPI :: Proxy MockAppBackendAPI
mockAppBackendAPI = Proxy

mockAppBackendServer :: V.Key (HashMap Text Text) -> FlowServer MockAppBackendAPI
mockAppBackendServer _key =
  pure "Mock app backend is UP"
    :<|> triggerSearchFlow
    :<|> onSearchFlow

type TriggerSearchAPI =
  "trigger"
    :> Get '[JSON] AckResponse

type OnSearchAPI =
  "on_search"
    :> APIKeyAuth VerifyAPIKey
    :> ReqBody '[JSON] OnSearchReq
    :> Post '[JSON] AckResponse

onSearchFlow :: FlowServer OnSearchAPI
onSearchFlow = P.searchCb

triggerSearchFlow :: FlowServer TriggerSearchAPI
triggerSearchFlow = P.triggerSearch
