module App.Routes where

import App.Types
import Beckn.Types.App (FlowServerR)
import qualified Data.Vault.Lazy as V
import EulerHS.Prelude
import qualified Product.Search as P
import Servant
import Types.API.Search

type GatewayAPI =
  "v1"
    :> ( Get '[JSON] Text
           :<|> SearchAPI
           :<|> OnSearchAPI
       )

gatewayAPI :: Proxy GatewayAPI
gatewayAPI = Proxy

gatewayServer :: V.Key (HashMap Text Text) -> FlowServerR AppEnv GatewayAPI
gatewayServer _key =
  pure "Gateway is UP"
    :<|> P.search
    :<|> P.searchCb
