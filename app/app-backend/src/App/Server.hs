module App.Server where

import App.Routes (appAPI, appServer)
import App.Types
import qualified Beckn.Utils.Servant.Server as BU
import qualified Data.Vault.Lazy as V
import EulerHS.Prelude
import Servant
import Utils.Common

run :: V.Key (HashMap Text Text) -> Env -> Application
run key = BU.run appAPI (appServer key) context
  where
    context = verifyPersonAction :. EmptyContext
