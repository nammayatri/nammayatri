module FerryBppApp
  ( runMockFerryBPP,
  )
where

import API.HealthCheck
import API.Init
import API.Search
import API.Types
import Beckn.Prelude
import Beckn.Utils.Dhall (readDhallConfigDefault)
import Network.Wai.Handler.Warp
  ( defaultSettings,
    runSettings,
    setPort,
  )
import Servant
import Types.App
import Types.Environment

--import API.Confirm

runMockFerryBPP :: IO ()
runMockFerryBPP = do
  appCfg <- readDhallConfigDefault "ferry-bpp" :: IO AppCfg
  let port = appCfg.port
      appEnv = buildAppEnv appCfg
      settings =
        defaultSettings & setPort port
  runSettings settings $
    run totalAPI totalServer appEnv

totalServer :: ServerT TotalAPI MockM
totalServer = healthCheckServer :<|> searchServer :<|> initServer -- :<|> confirmServer
