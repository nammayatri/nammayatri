module App
  ( runService,
  )
where

import API.Handler
import API.Types
import App.Types
import Beckn.Prelude
import Beckn.Utils.Dhall (readDhallConfigDefault)
import Beckn.Utils.Servant.Server (runServerWithHealthCheck)
import Servant (Context (..))

runService :: (AppCfg -> AppCfg) -> IO ()
runService configModifier = do
  defaultConfig <- readDhallConfigDefault "mock-dunzo" :: IO AppCfg
  appEnv <- buildAppEnv $ configModifier defaultConfig
  runServerWithHealthCheck appEnv (Proxy @API) handler identity identity EmptyContext releaseAppEnv pure
