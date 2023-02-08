{-# OPTIONS_GHC -Wno-deprecations #-}

module App.Server where

import API
import Core.Beckn (logBecknRequest)
import Environment
import EulerHS.Prelude
import Kernel.Tools.Metrics.Init
import Kernel.Types.Flow
import Kernel.Utils.App
import Kernel.Utils.Monitoring.Prometheus.Servant ()
import qualified Kernel.Utils.Servant.Server as BU
import Servant
import Tools.Auth

run :: Env -> Application
run = withModifiedEnv $ \modifiedEnv ->
  BU.run appAPI API.handler context modifiedEnv
    & logRequestAndResponse modifiedEnv
    & logBecknRequest modifiedEnv.appEnv
    & addServantInfo appAPI
    & hashBodyForSignature
    & supportProxyAuthorization
  where
    appAPI = Proxy @API.API
    context =
      verifyPersonAction @(FlowR AppEnv)
        :. verifyDashboardAction @(FlowR AppEnv)
        :. EmptyContext
