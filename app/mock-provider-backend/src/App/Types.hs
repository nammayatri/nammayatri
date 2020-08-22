{-# LANGUAGE TypeApplications #-}

module App.Types where

import Beckn.Storage.DB.Config
import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Utils.Dhall (FromDhall, ZL (..), z)
import EulerHS.Prelude
import Servant.Client (BaseUrl, Scheme)

data AppEnv = AppEnv
  { dbCfg :: DBConfig,
    port :: Int,
    xGatewayUri :: BaseUrl,
    selfId :: Maybe Text,
    nwAddress :: Maybe Text,
    migrationPath :: Maybe FilePath,
    autoMigrate :: Bool
  }
  deriving (Generic, FromDhall)

type Env = EnvR AppEnv

type Flow = FlowR AppEnv

type FlowHandler = FlowHandlerR AppEnv

type FlowServer api = FlowServerR AppEnv api

tyEnv :: ZL '[Scheme]
tyEnv = z @Scheme "UrlScheme" Z
