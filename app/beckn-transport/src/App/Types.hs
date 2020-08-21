{-# LANGUAGE TypeApplications #-}

module App.Types where

import Beckn.External.Exotel.Types (ExotelCfg)
import Beckn.Sms.Config (SmsConfig)
import Beckn.Storage.DB.Config (DBConfig)
import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Utils.Dhall (FromDhall, ZL (..), z)
import EulerHS.Prelude
import qualified EulerHS.Types as T
import Servant.Client (BaseUrl, Scheme)

data AppEnv = AppEnv
  { dbCfg :: DBConfig,
    redisCfg :: T.RedisConfig,
    smsCfg :: SmsConfig,
    port :: Int,
    metricsPort :: Int,
    xGatewayUri :: BaseUrl,
    xGatewayApiKey :: Maybe Text,
    xGatewaySelector :: Maybe Text,
    xGatewayNsdlUrl :: Maybe BaseUrl,
    nsdlUsername :: Maybe Text,
    nsdlPassword :: Maybe Text,
    xAppUri :: BaseUrl,
    selfId :: Maybe Text,
    nwAddress :: Maybe Text,
    caseExpiry :: Maybe Integer,
    cronAuthKey :: Maybe CronAuthKey,
    encService :: (String, Word16),
    fcmJsonPath :: Maybe Text,
    exotelCfg :: Maybe ExotelCfg,
    migrationPath :: Maybe FilePath,
    autoMigrate :: Bool
  }
  deriving (Generic, FromDhall)

type Env = EnvR AppEnv

type Flow = FlowR AppEnv

type FlowHandler = FlowHandlerR AppEnv

type FlowServer api = FlowServerR AppEnv api

tyEnv :: ZL '[Scheme, ExotelCfg]
tyEnv = z @Scheme "UrlScheme" $ z @ExotelCfg "ExotelCfg" Z
