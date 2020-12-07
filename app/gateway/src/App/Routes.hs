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
import Control.Concurrent.STM.TMVar (isEmptyTMVar)
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified Product.Log as P
import qualified Product.Search as P
import Servant
import Types.API.Search
import Utils.Auth (VerifyAPIKey, lookupRegistryAction)
import qualified Utils.Servant.SignatureAuth as HttpSig

type HealthAPI =
  "healthz" :> Get '[JSON] Text

type GatewayAPI' =
  "v1"
    :> ( Get '[JSON] Text
           :<|> SearchAPI VerifyAPIKey
           :<|> OnSearchAPI VerifyAPIKey
           :<|> LogAPI VerifyAPIKey
       )

type GatewayAPI = HealthAPI :<|> GatewayAPI'

gatewayAPI :: Proxy GatewayAPI
gatewayAPI = Proxy

gatewayServer :: TMVar () -> FlowServerR AppEnv GatewayAPI
gatewayServer shutdown =
  healthHandler :<|> gatewayHandler shutdown

healthHandler :: FlowServerR AppEnv HealthAPI
healthHandler = pure "UP"

gatewayHandler :: TMVar () -> FlowServerR AppEnv GatewayAPI'
gatewayHandler shutdown = do
  pure "Gateway is UP"
    :<|> (handleIfUp (HttpSig.withBecknAuth (P.search . Just) lookupRegistryAction) :<|> handleIfUp (P.search Nothing))
    :<|> (handleIfUp (HttpSig.withBecknAuth (P.searchCb . Just) lookupRegistryAction) :<|> handleIfUp (P.searchCb Nothing))
    :<|> handleIfUp P.log
  where
    handleIfUp :: (a -> b -> FlowHandler c) -> a -> b -> FlowHandler c
    handleIfUp handler a b = do
      shouldRun <- liftIO $ atomically $ isEmptyTMVar shutdown
      if shouldRun
        then handler a b
        else withFlowHandler $ L.throwException err503
