module App.Routes
  ( GatewayAPI,
    gatewayAPI,
    gatewayServer,
  )
where

import App.Types
import Beckn.Types.App (FlowServerR)
import EulerHS.Prelude
import qualified Product.Search as Search
import Servant hiding (throwError)
import Types.API.Search

-- TODO: unify these two into one
type HealthAPI =
  "healthz" :> Get '[JSON] Text

type GatewayAPI' =
  "v1"
    :> ( Get '[JSON] Text
           :<|> SearchAPI
           :<|> OnSearchAPI
       )

type GatewayAPI = HealthAPI :<|> GatewayAPI'

gatewayAPI :: Proxy GatewayAPI
gatewayAPI = Proxy

gatewayServer :: FlowServerR AppEnv GatewayAPI
gatewayServer =
  healthHandler :<|> gatewayHandler

healthHandler :: FlowServerR AppEnv HealthAPI
healthHandler =
  pure "UP"

gatewayHandler :: FlowServerR AppEnv GatewayAPI'
gatewayHandler =
  pure "Gateway is UP"
    :<|> Search.search
    :<|> Search.searchCb
