module App
  ( runMock,
  )
where

import API.Confirm
import API.Search
import API.Status
import API.Types
import Environment
import Kernel.Mock.App hiding (runMock)
import Kernel.Utils.App (logRequestAndResponseGeneric)
import Kernel.Utils.Dhall (readDhallConfigDefault)
import Kernel.Utils.Logging
import Network.Wai.Handler.Warp
  ( defaultSettings,
    runSettings,
    setPort,
  )
import Relude
import Servant

runMock :: (AppCfg -> AppCfg) -> IO ()
runMock cfgModifier = do
  appCfg <- cfgModifier <$> readDhallConfigDefault "mock-public-transport-provider-platform" :: IO AppCfg
  withAppEnv appCfg $ \appEnv -> do
    let port = appCfg.port
        settings =
          defaultSettings & setPort port
        reqRespLogger :: Text -> Text -> IO ()
        reqRespLogger tag info = runReaderT (runMockM $ withLogTag tag $ logOutput INFO info) appEnv

    runSettings settings $
      logRequestAndResponseGeneric reqRespLogger $
        run totalAPI totalServer appEnv

totalServer :: ServerT TotalAPI (MockM AppEnv)
totalServer = searchServer :<|> confirmServer :<|> statusServer
