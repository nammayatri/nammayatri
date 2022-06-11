module App.Server where

import App.Routes
import Beckn.Tools.Metrics.Init
import Beckn.Types.Flow
import Beckn.Utils.App
import qualified Beckn.Utils.Servant.Server as BU
import Environment
import EulerHS.Prelude
import Servant
import Utils.Auth

run :: Env -> Application
run = withModifiedEnv $ \modifiedEnv ->
  BU.run driverOfferAPI driverOfferServer context modifiedEnv
    & logRequestAndResponse modifiedEnv
    & addServantInfo driverOfferAPI
    & hashBodyForSignature
    & supportProxyAuthorization
  where
    context =
      verifyTokenAction @(FlowR AppEnv)
        :. validateAdminAction @(FlowR AppEnv)
        :. EmptyContext
