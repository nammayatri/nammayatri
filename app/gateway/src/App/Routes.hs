module App.Routes where

import App.Types
import qualified Beckn.Types.API.Search as Search
import Beckn.Types.App (AuthHeader, FlowServerR)
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

gatewayServer :: V.Key (HashMap Text Text) -> FlowServerR AppEnv GatewayAPI
gatewayServer _key =
  pure "Gateway is UP"
    :<|> searchFlow

type SearchAPI =
  AuthHeader
    :> ( Search.SearchAPI
           :<|> Search.OnSearchAPI
       )

searchFlow :: FlowServerR AppEnv SearchAPI
searchFlow token =
  P.search token
    :<|> P.searchCb token
