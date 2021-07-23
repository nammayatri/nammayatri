module App.Types where

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
  { smsMap :: MVar (Map.Map MobileNumber [Text])
  }
  deriving (Generic)

buildAppEnv :: AppCfg -> IO AppEnv
buildAppEnv AppCfg {..} = do
  smsMap <- newMVar Map.empty
  return $ AppEnv {smsMap}

type FlowHandler = FlowHandlerR AppEnv

type FlowServer r api = FlowServerR AppEnv api
