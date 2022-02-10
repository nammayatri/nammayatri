module App
  ( runMockPublicTransportBPP,
  )
where

import API.Confirm
import API.HealthCheck
import API.Search
import API.Status
import API.Types
import Beckn.Mock.App
import Beckn.Utils.App (logRequestAndResponseGeneric)
import Beckn.Utils.Dhall (readDhallConfigDefault)
import Beckn.Utils.Logging
import Environment
import Network.Wai.Handler.Warp
  ( defaultSettings,
    runSettings,
    setPort,
  )
import Relude
import Servant

runMockPublicTransportBPP :: IO ()
runMockPublicTransportBPP = do
  appCfg <- readDhallConfigDefault "mock-public-transport-bpp" :: IO AppCfg
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
totalServer = healthCheckServer :<|> searchServer :<|> confirmServer :<|> statusServer
