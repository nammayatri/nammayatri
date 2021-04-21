module App.Routes
  ( GatewayAPI,
    gatewayAPI,
    gatewayServer,
  )
where

import App.Types
import Beckn.Types.App (FlowServerR)
import Beckn.Types.Core.API.Log
import Beckn.Utils.Servant.SignatureAuth (lookupRegistryAction)
import EulerHS.Prelude
import qualified Product.Log as P
import qualified Product.Search as P
import Servant hiding (throwError)
import Storage.Queries.Organization
import Types.API.Search
import Utils.Auth (VerifyAPIKey)
import qualified Utils.Servant.SignatureAuth as HttpSig

type HealthAPI =
  "healthz" :> Get '[JSON] Text

type GatewayAPI' =
  "v1"
    :> ( Get '[JSON] Text
           :<|> SearchAPI
           :<|> OnSearchAPI
           :<|> LogAPI VerifyAPIKey
       )

type GatewayAPI = HealthAPI :<|> GatewayAPI'

gatewayAPI :: Proxy GatewayAPI
gatewayAPI = Proxy

gatewayServer :: FlowServerR AppEnv GatewayAPI
gatewayServer =
  healthHandler :<|> gatewayHandler

healthHandler :: FlowServerR AppEnv HealthAPI
healthHandler = pure "UP"

gatewayHandler :: FlowServerR AppEnv GatewayAPI'
gatewayHandler = do
  pure "Gateway is UP"
    :<|> HttpSig.withBecknAuthProxy P.search lookup
    :<|> HttpSig.withBecknAuthProxy P.searchCb lookup
    :<|> P.log
  where
    lookup = lookupRegistryAction findOrgByShortId
