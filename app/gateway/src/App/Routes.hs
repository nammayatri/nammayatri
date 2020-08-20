module App.Routes where

import App.Types
import Beckn.Types.API.Log
import Beckn.Types.App (FlowServerR)
import EulerHS.Prelude
import qualified Product.Log as P
import qualified Product.Search as P
import Servant
import Types.API.Search
import Utils.Auth (VerifyAPIKey)

type GatewayAPI =
  "v1"
    :> ( Get '[JSON] Text
           :<|> SearchAPI
           :<|> OnSearchAPI
           :<|> LogAPI VerifyAPIKey
       )

gatewayAPI :: Proxy GatewayAPI
gatewayAPI = Proxy

gatewayServer :: FlowServerR AppEnv GatewayAPI
gatewayServer =
  pure "Gateway is UP"
    :<|> P.search
    :<|> P.searchCb
    :<|> P.log
