module App.Server where

import App.Routes (transporterAPI, transporterServer)
import App.Types
import Beckn.Types.App
import qualified Beckn.Utils.Servant.Server as BU
import qualified Data.Vault.Lazy as V
import EulerHS.Prelude
import Servant

run :: V.Key (HashMap Text Text) -> Env -> Application
run key = BU.run transporterAPI (transporterServer key)
