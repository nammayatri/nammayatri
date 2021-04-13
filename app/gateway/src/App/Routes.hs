module App.Routes
  ( GatewayAPI,
    gatewayAPI,
    gatewayServer,
  )
where

import App.Types
import Beckn.Types.App (FlowServerR)
import Beckn.Types.Core.API.Log
import Beckn.Utils.Common (withFlowHandler)
import Beckn.Utils.Servant.SignatureAuth (lookupRegistryAction)
import Control.Concurrent.STM.TMVar (isEmptyTMVar)
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified Product.Log as P
import qualified Product.Search as P
import Servant
import Storage.Queries.Organization
import System.Exit (ExitCode)
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

gatewayServer :: TMVar ExitCode -> FlowServerR AppEnv GatewayAPI
gatewayServer shutdown =
  healthHandler :<|> gatewayHandler shutdown

healthHandler :: FlowServerR AppEnv HealthAPI
healthHandler = pure "UP"

gatewayHandler :: TMVar ExitCode -> FlowServerR AppEnv GatewayAPI'
gatewayHandler shutdown = do
  pure "Gateway is UP"
    :<|> handleIfUp (HttpSig.withBecknAuthProxy P.search lookup)
    :<|> handleIfUp (HttpSig.withBecknAuthProxy P.searchCb lookup)
    :<|> handleIfUp P.log
  where
    handleIfUp :: (a -> b -> FlowHandler c) -> a -> b -> FlowHandler c
    handleIfUp handler a b = do
      shouldRun <- liftIO $ atomically $ isEmptyTMVar shutdown
      if shouldRun
        then handler a b
        else withFlowHandler $ L.throwException err503

    lookup = lookupRegistryAction findOrgByShortId
