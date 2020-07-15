module App.Routes where

import App.Types
import qualified Beckn.Types.API.Search as Search
import Beckn.Types.App (APIKey, FlowServerR)
import Beckn.Utils.Servant.API
import qualified Data.Vault.Lazy as V
import EulerHS.Prelude
import EulerHS.Types (EulerClient, client)
import qualified Product.Search as P
import Servant
import Servant.Client (Client)
import Utils.Auth

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
  APIKeyAuth
    :>| Search.SearchAPI
    :<|> Search.OnSearchAPI

searchFlow :: FlowServerR AppEnv SearchAPI
searchFlow =
  P.search
    :<|> P.searchCb

cliHealthCheck :: EulerClient Text
cliSearch :: APIKey -> Client EulerClient Search.SearchAPI
cliOnSearch :: APIKey -> Client EulerClient Search.OnSearchAPI
cliHealthCheck :<|> (cliSearch :<|> cliOnSearch) = client gatewayAPI
