{-# LANGUAGE QuantifiedConstraints #-}

module App
  ( runMockSms,
  )
where

import App.Server
import App.Types
import qualified Beckn.Types.App as App
import Beckn.Types.Flow
import Beckn.Types.Logging
import qualified Beckn.Utils.FlowLogging as L
import Beckn.Utils.Logging
import qualified Data.Text as T
import EulerHS.Prelude hiding (exitSuccess)
import EulerHS.Runtime as E
import Network.Wai.Handler.Warp
  ( defaultSettings,
    runSettings,
    setInstallShutdownHandler,
    setPort,
  )
import System.Environment (lookupEnv)

runMockSms :: (AppCfg -> AppCfg) -> IO ()
runMockSms configModifier = do
  let appCfg = configModifier defaultConfig
  let port = appCfg.port
  appEnv <- buildAppEnv appCfg
  hostname <- (T.pack <$>) <$> lookupEnv "POD_NAME"
  let loggerRt = L.getEulerLoggerRuntime hostname $ appCfg.loggerConfig
      settings =
        defaultSettings
          & setInstallShutdownHandler (shutdownAction appEnv)
          & setPort port
  E.withFlowRuntime (Just loggerRt) $ \flowRt -> do
    runFlowR flowRt appEnv $ logInfo ("Runtime created. Starting server at port " <> show port)
    runSettings settings $ run (App.EnvR flowRt appEnv)
  where
    shutdownAction appEnv closeSocket = do
      releaseAppEnv appEnv
      closeSocket

defaultConfig :: AppCfg
defaultConfig =
  AppCfg
    { port = 4343,
      loggerConfig =
        LoggerConfig
          { level = DEBUG,
            logToFile = True,
            logFilePath = "/tmp/mock-sms.log",
            logToConsole = True,
            logRawSql = True
          }
    }
