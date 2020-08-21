module App.Types where

import Beckn.Storage.DB.Config (DBConfig)
import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Utils.Dhall (FromDhall)
import qualified Data.Cache as C
import EulerHS.Prelude

data AppCfg = AppCfg
  { dbCfg :: DBConfig,
    port :: Int,
    metricsPort :: Int,
    selfId :: Maybe Text,
    nwAddress :: Maybe Text
  }
  deriving (Generic, FromDhall)

data AppEnv = AppEnv
  { dbCfg :: DBConfig,
    gwId :: Maybe Text,
    gwNwAddress :: Maybe Text,
    cache :: C.Cache Text Text
  }

mkAppEnv :: AppCfg -> C.Cache Text Text -> AppEnv
mkAppEnv AppCfg {..} c = AppEnv {gwId = selfId, gwNwAddress = nwAddress, cache = c, ..}

type Flow = FlowR AppEnv

type FlowHandler = FlowHandlerR AppEnv

type FlowServer r api = FlowServerR AppEnv api
