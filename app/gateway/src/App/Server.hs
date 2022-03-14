module App.Server
  ( run,
  )
where

import App.Routes (gatewayAPI, gatewayServer)
import App.Types
import Beckn.Tools.Metrics.Init
import Beckn.Types.App
import Beckn.Utils.App
import qualified Beckn.Utils.Servant.Server as BU
import EulerHS.Prelude
import Servant

run :: EnvR AppEnv -> Application
run = withModifiedEnv $ \modifiedEnv ->
  BU.run gatewayAPI gatewayServer context modifiedEnv
    & addServantInfo gatewayAPI
    & hashBodyForSignature
    & supportProxyAuthorization
  where
    context = EmptyContext
