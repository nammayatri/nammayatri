module App.Server
  ( run,
  )
where

import App.Handlers (fmdWrapperBackendServer, wrapperAPI)
import App.Types
import Beckn.Utils.App
import qualified Beckn.Utils.Servant.Server as BU
import EulerHS.Prelude
import Servant
import Utils.Auth

run :: Env -> Application
run = withModifiedEnv $ \modifiedEnv ->
  logRequestAndResponse modifiedEnv $
    BU.run wrapperAPI fmdWrapperBackendServer context modifiedEnv
  where
    context = verifyApiKey :. EmptyContext
