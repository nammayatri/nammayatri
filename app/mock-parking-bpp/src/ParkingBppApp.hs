module ParkingBppApp
  ( runMockParkingBPP,
  )
where

import API.Confirm
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

runMockParkingBPP :: IO ()
runMockParkingBPP = do
  appCfg <- readDhallConfigDefault "mock-parking-bpp" :: IO AppCfg
  withAppEnv appCfg $ \appEnv -> do
    let port = appCfg.port
        settings =
          defaultSettings & setPort port
        reqRespLogger :: Text -> Text -> IO ()
        reqRespLogger tag info = runReaderT (runMockM $ withLogTag tag $ logInfo info) appEnv

    runSettings settings $
      logRequestAndResponseGeneric reqRespLogger $
        run totalAPI totalServer appEnv

totalServer :: ServerT TotalAPI (MockM AppEnv)
totalServer = searchServer :<|> confirmServer :<|> statusServer
