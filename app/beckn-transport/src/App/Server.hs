module App.Server where

import App.Routes (transporterAPI, transporterServer)
import App.Types
import Beckn.Utils.Monitoring.Prometheus.Metrics (addServantInfo)
import qualified Beckn.Utils.Servant.Server as BU
import qualified Data.Vault.Lazy as V
import EulerHS.Prelude
import Servant
import Utils.Auth
import Utils.Common

run :: V.Key (HashMap Text Text) -> Env -> Application
run key env = addServantInfo transporterAPI $ BU.run transporterAPI (transporterServer key) context env
  where
    context =
      verifyApiKey
        :. verifyTokenAction
        :. verifyOrgAction
        :. validateAdminAction
        :. validateDriverAction
        :. EmptyContext
