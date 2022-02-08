module ParkingBppApp
  ( runMockParkingBPP,
  )
where

import API.Confirm
import API.HealthCheck
import API.Search
import API.Status
import API.Types
import Beckn.Mock.App
import Beckn.Utils.App (logRequestAndResponseGeneric)
import Beckn.Utils.CacheHedis
import Beckn.Utils.Dhall (readDhallConfigDefault)
import Beckn.Utils.IOLogging
import Beckn.Utils.Logging
import qualified Control.Monad.Catch as C
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
  withHedisEnv $ \hedisEnv -> do
    withIOLogger appCfg.loggerConfig $ \loggerEnv -> do
      let port = appCfg.port
          appEnv = buildAppEnv hedisEnv loggerEnv appCfg
          settings =
            defaultSettings & setPort port
          reqRespLogger :: Text -> Text -> IO ()
          reqRespLogger tag info = runReaderT (runMockM $ withLogTag tag $ logOutput INFO info) appEnv
      runSettings settings $
        logRequestAndResponseGeneric reqRespLogger $
          run totalAPI totalServer appEnv

withIOLogger :: LoggerConfig -> (LoggerEnv -> IO ()) -> IO ()
withIOLogger conf = C.bracket (prepareLoggerEnv conf Nothing) releaseLoggerEnv

totalServer :: ServerT TotalAPI (MockM AppEnv)
totalServer = healthCheckServer :<|> searchServer :<|> confirmServer :<|> statusServer
