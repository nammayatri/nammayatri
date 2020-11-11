module App.Server
  ( run,
  )
where

import App.Handlers (mockProviderBackendServer, providerAPI)
import App.Types
import Beckn.Utils.Monitoring.Prometheus.Metrics
import qualified Beckn.Utils.Servant.Server as BU
import EulerHS.Prelude
import Servant
import Utils.Auth

run :: Env -> Application
run env =
  addServantInfo providerAPI $
    BU.run providerAPI mockProviderBackendServer context env
  where
    context = verifyApiKey :. EmptyContext
