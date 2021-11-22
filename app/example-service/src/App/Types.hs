module App.Types where

import Beckn.Prelude
import Beckn.Types.Common
import Beckn.Utils.Dhall (FromDhall)
import Control.Concurrent.MVar

data AppCfg = AppCfg
  { port :: Int,
    loggerConfig :: LoggerConfig,
    graceTerminationPeriod :: Seconds
  }
  deriving (Generic, FromDhall)

type MobileNumber = Text

data AppEnv = AppEnv
  { config :: AppCfg,
    isShuttingDown :: MVar ()
  }
  deriving (Generic)

buildAppEnv :: AppCfg -> IO AppEnv
buildAppEnv config@AppCfg {..} = do
  isShuttingDown <- newEmptyMVar
  return $ AppEnv {..}

type FlowHandler = FlowHandlerR AppEnv

type FlowServer r api = FlowServerR AppEnv api
