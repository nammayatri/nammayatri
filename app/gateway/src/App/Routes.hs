module App.Routes where

import App.Types
import qualified Beckn.Types.API.Search as Search
import Beckn.Types.App (AuthHeader, FlowServerR, RegToken)
import Beckn.Utils.Servant.API
import qualified Data.Vault.Lazy as V
import EulerHS.Prelude
import EulerHS.Types (EulerClient, client)
import qualified Product.Search as P
import Servant
import Servant.Client (Client)

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
    :>| Search.SearchAPI
    :<|> Search.OnSearchAPI

searchFlow :: FlowServerR AppEnv SearchAPI
searchFlow =
  P.search
    :<|> P.searchCb

cliHealthCheck :: EulerClient Text
cliSearch :: RegToken -> Client EulerClient Search.SearchAPI
cliOnSearch :: RegToken -> Client EulerClient Search.OnSearchAPI
cliHealthCheck :<|> (cliSearch :<|> cliOnSearch) = client gatewayAPI
