module App.Server
  ( run,
  )
where

import App.Handlers (fmdWrapperBackendServer, wrapperAPI)
import App.Types
import qualified Beckn.Utils.Servant.Server as BU
import Servant
import Utils.Auth

run :: Env -> Application
run = BU.run wrapperAPI fmdWrapperBackendServer context
  where
    context = verifyApiKey :. EmptyContext
