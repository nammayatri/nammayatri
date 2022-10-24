module App
  ( runMock,
  )
where

import API.Total
import Beckn.Mock.App hiding (runMock)
import Beckn.Prelude
import Beckn.Types.Common (defaultLoggerConfig)
import Beckn.Utils.App (logRequestAndResponseGeneric)
import Beckn.Utils.Logging
import Environment
import Network.Wai.Handler.Warp
  ( defaultSettings,
    runSettings,
    setPort,
  )

runMock :: (AppCfg -> AppCfg) -> IO ()
runMock _cfgModifier = do
  let appCfg = defaultAppCfg
  withAppEnv appCfg $ \appEnv -> do
    let port = appCfg.port
        settings =
          defaultSettings & setPort port
        reqRespLogger :: Text -> Text -> IO ()
        reqRespLogger tag info = runReaderT (runMockM $ withLogTag tag $ logOutput INFO info) appEnv

    runSettings settings $
      logRequestAndResponseGeneric reqRespLogger $
        run totalAPI totalServer appEnv

defaultAppCfg :: AppCfg
defaultAppCfg =
  AppCfg
    { port = 8099,
      loggerConfig = defaultLoggerConfig
    }
