module App.Types where

import Beckn.External.FCM.Types
import Beckn.Types.App
import Beckn.Types.Common hiding (id)
import Beckn.Utils.Dhall (FromDhall)
import qualified Data.Map as Map
import EulerHS.Prelude

data AppCfg = AppCfg
  { port :: Int,
    loggerConfig :: LoggerConfig
  }
  deriving (Generic, FromDhall)

type MobileNumber = Text

newtype AppEnv = AppEnv
  { notificationsMap :: MVar (Map.Map FCMRecipientToken [FCMMessage])
  }
  deriving (Generic)

buildAppEnv :: AppCfg -> IO AppEnv
buildAppEnv AppCfg {..} = do
  notificationsMap <- newMVar Map.empty
  return $ AppEnv {notificationsMap}

type FlowHandler = FlowHandlerR AppEnv

type FlowServer r api = FlowServerR AppEnv api
