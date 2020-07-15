module App.Server
  ( run,
  )
where

import App.Routes (mockAppBackendAPI, mockAppBackendServer)
import App.Types
import qualified Beckn.Utils.Servant.Server as BU
import qualified Data.Vault.Lazy as V
import EulerHS.Prelude
import Servant

run :: V.Key (HashMap Text Text) -> Env -> Application
run key = BU.run mockAppBackendAPI (mockAppBackendServer key) EmptyContext
