module App.Server
  ( run,
  )
where

import App.Routes (gatewayAPI, gatewayServer)
import App.Types
import Beckn.Types.App
import qualified Beckn.Utils.Servant.Server as BU
import qualified Data.Vault.Lazy as V
import EulerHS.Prelude
import Servant
import Utils.Auth

run :: V.Key (HashMap Text Text) -> EnvR AppEnv -> Application
run key = BU.run gatewayAPI (gatewayServer key) context
  where
    context = verifyAPIKeyAction :. EmptyContext
