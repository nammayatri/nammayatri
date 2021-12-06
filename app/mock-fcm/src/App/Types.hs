module App.Types where

import Beckn.External.FCM.Types
import Beckn.Types.App
import Beckn.Types.Common hiding (id)
import Beckn.Utils.Dhall (FromDhall)
import Beckn.Utils.IOLogging
import qualified Data.Map as Map
import qualified Data.Text as T
import EulerHS.Prelude
import System.Environment (lookupEnv)

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
  hostname <- map T.pack <$> lookupEnv "POD_NAME"
  notificationsMap <- newMVar Map.empty
  loggerEnv <- prepareLoggerEnv loggerConfig hostname
  return $ AppEnv {..}

releaseAppEnv :: AppEnv -> IO ()
releaseAppEnv AppEnv {..} =
  releaseLoggerEnv loggerEnv

type FlowHandler = FlowHandlerR AppEnv

type FlowServer r api = FlowServerR AppEnv api
