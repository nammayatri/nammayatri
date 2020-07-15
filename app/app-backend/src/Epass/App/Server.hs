module Epass.App.Server where

import App.Types
import qualified Beckn.Utils.Servant.Server as BU
import qualified Data.Vault.Lazy as V
import Epass.App.Routes (epassAPI, epassServer)
import Epass.Types.App
import EulerHS.Prelude
import Servant

run :: V.Key (HashMap Text Text) -> Env -> Application
run key = BU.run epassAPI (epassServer key) EmptyContext
