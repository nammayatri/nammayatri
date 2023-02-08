module App
  ( runMock,
  )
where

import API.Total
import Environment
import Kernel.Mock.App hiding (runMock)
import Kernel.Prelude
import Kernel.Types.Common (defaultLoggerConfig)
import Kernel.Utils.App (logRequestAndResponseGeneric)
import Kernel.Utils.Logging
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
