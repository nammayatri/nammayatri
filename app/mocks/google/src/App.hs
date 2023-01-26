module App
  ( runService,
  )
where

import API
import Beckn.Prelude
import Beckn.Utils.Dhall (readDhallConfigDefault)
import Beckn.Utils.Servant.Server (runServerWithHealthCheck)
import qualified Data.Map.Strict as Map
import Environment
import qualified EulerHS.Runtime as R
import Servant (Context (..))

runService :: (AppCfg -> AppCfg) -> IO ()
runService cfgModifier = do
  appCfg <- cfgModifier <$> readDhallConfigDefault "mock-google" :: IO AppCfg
  appEnv <- buildAppEnv appCfg
  runServerWithHealthCheck appEnv (Proxy @API) handler identity identity EmptyContext releaseAppEnv \flowRt -> do
    pure flowRt {R._httpClientManagers = Map.singleton "default" (R._defaultHttpClientManager flowRt)}
