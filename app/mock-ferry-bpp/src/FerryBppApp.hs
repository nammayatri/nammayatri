module FerryBppApp
  ( runMockFerryBPP,
  )
where

import API.Confirm
import API.HealthCheck
import API.Search
import API.Status
import API.Types
import Beckn.Prelude
import Beckn.Utils.Dhall (readDhallConfigDefault)
import Network.Wai.Handler.Warp
  ( defaultSettings,
    runSettings,
    setPort,
  )
import Redis (withRedisConnection)
import Servant
import Types.App
import Types.Environment

runMockFerryBPP :: IO ()
runMockFerryBPP = do
  appCfg <- readDhallConfigDefault "ferry-bpp" :: IO AppCfg
  withRedisConnection $ \redisCon -> do
    let port = appCfg.port
        appEnv = buildAppEnv redisCon appCfg
        settings =
          defaultSettings & setPort port
    runSettings settings $
      run totalAPI totalServer appEnv

totalServer :: ServerT TotalAPI MockM
totalServer = healthCheckServer :<|> searchServer :<|> confirmServer :<|> statusServer
