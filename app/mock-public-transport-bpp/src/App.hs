module App
  ( runMockFerryBPP,
  )
where

import API.Confirm
import API.HealthCheck
import API.Search
import API.Status
import API.Types
import Beckn.Mock.App
import Beckn.Utils.CacheHedis
import Beckn.Utils.Dhall (readDhallConfigDefault)
import Beckn.Utils.IOLogging
import qualified Control.Monad.Catch as C
import Environment
import Network.Wai.Handler.Warp
  ( defaultSettings,
    runSettings,
    setPort,
  )
import Relude
import Servant

runMockFerryBPP :: IO ()
runMockFerryBPP = do
  appCfg <- readDhallConfigDefault "mock-public-transport-bpp" :: IO AppCfg
  withHedisEnv $ \hedisEnv -> do
    withIOLogger appCfg.loggerConfig $ \loggerEnv -> do
      let port = appCfg.port
          appEnv = buildAppEnv hedisEnv loggerEnv appCfg
          settings =
            defaultSettings & setPort port
      runSettings settings $
        run totalAPI totalServer appEnv

withIOLogger :: LoggerConfig -> (LoggerEnv -> IO ()) -> IO ()
withIOLogger conf = C.bracket (prepareLoggerEnv conf Nothing) releaseLoggerEnv

totalServer :: ServerT TotalAPI (MockM AppEnv)
totalServer = healthCheckServer :<|> searchServer :<|> confirmServer :<|> statusServer
