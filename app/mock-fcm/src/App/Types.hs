module App.Types where

import Beckn.External.FCM.Types
import Beckn.Types.App
import Beckn.Types.Common hiding (id)
import Beckn.Utils.App (getPodName)
import Beckn.Utils.Dhall (FromDhall)
import Beckn.Utils.IOLogging
import qualified Data.Map as Map
import EulerHS.Prelude

data AppCfg = AppCfg
  { port :: Int,
    loggerConfig :: LoggerConfig
  }
  deriving (Generic, FromDhall)

type MobileNumber = Text

data AppEnv = AppEnv
  { notificationsMap :: MVar (Map.Map FCMRecipientToken [FCMMessage]),
    loggerEnv :: LoggerEnv
  }
  deriving (Generic)

buildAppEnv :: AppCfg -> IO AppEnv
buildAppEnv AppCfg {..} = do
  hostname <- getPodName
  notificationsMap <- newMVar Map.empty
  loggerEnv <- prepareLoggerEnv loggerConfig hostname
  return $ AppEnv {..}

releaseAppEnv :: AppEnv -> IO ()
releaseAppEnv AppEnv {..} =
  releaseLoggerEnv loggerEnv

type FlowHandler = FlowHandlerR AppEnv

type FlowServer r api = FlowServerR AppEnv api
