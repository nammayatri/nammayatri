module App
  ( runService,
  )
where

import API.Handler
import API.Types
import App.Types
import Beckn.Prelude
import Beckn.Utils.Dhall (readDhallConfigDefault)
import Beckn.Utils.Servant.Server (runServerService)
import Servant (Context (..))

runService :: (AppCfg -> AppCfg) -> IO ()
runService configModifier = do
  appEnv <-
    readDhallConfigDefault "public-transport-bap"
      <&> configModifier
      >>= buildAppEnv
  runServerService appEnv (Proxy @API) handler identity identity EmptyContext releaseAppEnv pure
