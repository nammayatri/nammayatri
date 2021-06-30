{-# LANGUAGE TypeApplications #-}

module App.Server where

import App.Routes (appAPI, appServer)
import App.Types
import Beckn.Types.Flow
import Beckn.Utils.App
import Beckn.Utils.Monitoring.Prometheus.Metrics
import qualified Beckn.Utils.Servant.Server as BU
import EulerHS.Prelude
import Servant
import Utils.Auth

run :: Env -> Application
run = withModifiedEnv $ \modifiedEnv ->
  BU.run appAPI appServer context modifiedEnv
    & logRequestAndResponse modifiedEnv
    & addServantInfo appAPI
    & hashBodyForSignature
  where
    context =
      verifyApiKey @(FlowR AppEnv)
        :. verifyPersonAction @(FlowR AppEnv)
        :. EmptyContext
