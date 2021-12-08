module App.Types where

import Beckn.Types.App
import Beckn.Types.Common hiding (id)
import Beckn.Utils.Dhall (FromDhall)
import Beckn.Utils.IOLogging
import Beckn.Utils.Shutdown
import qualified Data.Map as Map
import qualified Data.Text as T
import EulerHS.Prelude
import System.Environment (lookupEnv)

data AppCfg = AppCfg
  { port :: Int,
    loggerConfig :: LoggerConfig,
    graceTerminationPeriod :: Seconds
  }
  deriving (Generic, FromDhall)

type MobileNumber = Text

data AppEnv = AppEnv
  { config :: AppCfg,
    smsMap :: MVar (Map.Map MobileNumber [Text]),
    isShuttingDown :: Shutdown,
    loggerEnv :: LoggerEnv
  }
  deriving (Generic)

buildAppEnv :: AppCfg -> IO AppEnv
buildAppEnv config@AppCfg {..} = do
  hostname <- map T.pack <$> lookupEnv "POD_NAME"
  smsMap <- newMVar Map.empty
  loggerEnv <- prepareLoggerEnv loggerConfig hostname
  isShuttingDown <- mkShutdown
  return $ AppEnv {..}

releaseAppEnv :: AppEnv -> IO ()
releaseAppEnv AppEnv {..} =
  releaseLoggerEnv loggerEnv

type FlowHandler = FlowHandlerR AppEnv

type FlowServer api = FlowServerR AppEnv api
