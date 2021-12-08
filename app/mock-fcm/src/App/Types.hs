module App.Types where

import Beckn.External.FCM.Types
import Beckn.Types.App
import Beckn.Types.Common hiding (id)
import Beckn.Utils.App (getPodName)
import Beckn.Utils.Dhall (FromDhall)
import Beckn.Utils.IOLogging
import qualified Data.Map as Map
import EulerHS.Prelude
import Beckn.Utils.Shutdown

data AppCfg = AppCfg
  { port :: Int,
    loggerConfig :: LoggerConfig,
    graceTerminationPeriod :: Seconds
  }
  deriving (Generic, FromDhall)

type MobileNumber = Text

data AppEnv = AppEnv
  { config :: AppCfg,
    notificationsMap :: MVar (Map.Map FCMRecipientToken [FCMMessage]),
    isShuttingDown :: Shutdown,
    loggerEnv :: LoggerEnv
  }
  deriving (Generic)

buildAppEnv :: AppCfg -> IO AppEnv
buildAppEnv config@AppCfg {..} = do
  hostname <- getPodName
  notificationsMap <- newMVar Map.empty
  loggerEnv <- prepareLoggerEnv loggerConfig hostname
  isShuttingDown <- mkShutdown
  return $ AppEnv {..}

releaseAppEnv :: AppEnv -> IO ()
releaseAppEnv AppEnv {..} =
  releaseLoggerEnv loggerEnv

type FlowHandler = FlowHandlerR AppEnv

type FlowServer api = FlowServerR AppEnv api
