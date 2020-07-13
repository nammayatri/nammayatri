module App.Server
  ( run,
  )
where

import App.Handlers (mockProviderBackendServer)
import Beckn.Types.App
import qualified Beckn.Utils.Servant.Server as BU
import qualified Data.Vault.Lazy as V
import EulerHS.Prelude
import "beckn-gateway" External.Provider.Routes
import Servant

run :: V.Key (HashMap Text Text) -> Env -> Application
run key = BU.run providerAPI (mockProviderBackendServer key)
