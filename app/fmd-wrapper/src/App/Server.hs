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
run env = \req resp -> do
  modifiedEnv <- modifyEnvR env
  let app =
        logRequestAndResponse modifiedEnv $
          BU.run wrapperAPI fmdWrapperBackendServer context modifiedEnv
  app req resp
  where
    context = verifyApiKey :. EmptyContext
