module App.Server where

import App.Routes (lookupFlow, registryAPI)
import App.Types (Env)
import Beckn.Utils.App (hashBodyForSignature)
import qualified Beckn.Utils.Servant.Server as BU
import EulerHS.Prelude
import Servant (Application, Context (..))

runServer :: Env -> Application
runServer env =
  BU.run registryAPI lookupFlow context env
    & hashBodyForSignature
  where
    context = EmptyContext
