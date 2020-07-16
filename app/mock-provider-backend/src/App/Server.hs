module App.Server
  ( run,
  )
where

import App.Handlers (mockProviderBackendServer, providerAPI)
import App.Types
import qualified Beckn.Utils.Servant.Server as BU
import qualified Data.Vault.Lazy as V
import EulerHS.Prelude
import Servant
import Utils.Auth

run :: V.Key (HashMap Text Text) -> Env -> Application
run key = BU.run providerAPI (mockProviderBackendServer key) context
  where
    context = verifyApiKey :. EmptyContext
