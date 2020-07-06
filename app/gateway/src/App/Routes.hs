module App.Routes where

import qualified Beckn.Types.API.Search as Search
import Beckn.Types.App (FlowServer)
import qualified Data.Vault.Lazy as V
import EulerHS.Prelude
import qualified Product.Search as P
import Servant

type GatewayAPI =
  "v1"
    :> ( Get '[JSON] Text
           :<|> SearchAPI
       )

gatewayAPI :: Proxy GatewayAPI
gatewayAPI = Proxy

gatewayServer :: V.Key (HashMap Text Text) -> FlowServer GatewayAPI
gatewayServer key =
  pure "Gateway is UP"
    :<|> searchFlow

type SearchAPI =
  "search" :> "services"
    :> ReqBody '[JSON] Search.SearchReq
    :> Post '[JSON] Search.SearchRes
    :<|> "on_search"
      :> "services"
      :> ReqBody '[JSON] Search.OnSearchReq
      :> Post '[JSON] Search.OnSearchRes

searchFlow :: FlowServer SearchAPI
searchFlow =
  P.search
    :<|> P.searchCb
