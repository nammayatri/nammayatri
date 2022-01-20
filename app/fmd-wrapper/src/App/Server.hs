module App.Server
  ( run,
  )
where

import API.Handler (handler)
import API.Types (API)
import App.Types
import Beckn.Utils.App
import qualified Beckn.Utils.Servant.Server as BU
import EulerHS.Prelude
import Servant

run :: Env -> Application
run = withModifiedEnv $ \modifiedEnv ->
  BU.run (Proxy :: (Proxy API)) handler context modifiedEnv
    & logRequestAndResponse modifiedEnv
    & hashBodyForSignature
    & supportProxyAuthorization
  where
    context = EmptyContext
