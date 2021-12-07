module App where

import App.Routes (lookupFlow, registryAPI)
import App.Types
import Beckn.Prelude
import Beckn.Utils.App
import Beckn.Utils.Dhall (readDhallConfigDefault)
import Beckn.Utils.Servant.Server (runServerService)
import Servant (Context (..))

runRegistryService :: (AppCfg -> AppCfg) -> IO ()
runRegistryService configModifier = do
  appEnv <- readDhallConfigDefault "mock-registry" <&> configModifier >>= buildAppEnv
  runServerService appEnv registryAPI lookupFlow middleware identity EmptyContext releaseAppEnv pure
  where
    middleware = hashBodyForSignature
