module App.Server
  ( run,
  )
where

import App.Handlers (mockProviderBackendServer, providerAPI)
import App.Types
import Beckn.Utils.Monitoring.Prometheus.Metrics
import qualified Beckn.Utils.Servant.Server as BU
import qualified Data.Vault.Lazy as V
import EulerHS.Prelude
import Servant
import Utils.Auth

run :: V.Key (HashMap Text Text) -> Env -> Application
run key env =
  addServantInfo providerAPI $
    BU.run providerAPI (mockProviderBackendServer key) context env
  where
    context = verifyApiKey :. EmptyContext
