module App.Types where

import Beckn.External.Exotel.Types (ExotelCfg)
import Beckn.SesConfig (SesConfig)
import Beckn.Sms.Config (SmsConfig)
import Beckn.Storage.DB.Config (DBConfig)
import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Utils.Dhall (FromDhall)
import Beckn.Utils.Logging
import Data.Time (NominalDiffTime)
import EulerHS.Prelude
import Types.Geofencing

data AppEnv = AppEnv
  { dbCfg :: DBConfig,
    smsCfg :: SmsConfig,
    sesCfg :: SesConfig,
    port :: Int,
    metricsPort :: Int,
    xGatewayUri :: BaseUrl,
    xGatewayApiKey :: Maybe Text,
    xGatewaySelector :: Maybe Text,
    xGatewayNsdlUrl :: Maybe BaseUrl,
    xProviderUri :: BaseUrl,
    bapSelfId :: Maybe Text,
    bapNwAddress :: Maybe BaseUrl,
    searchConfirmExpiry :: Maybe Integer,
    searchCaseExpiry :: Maybe Integer,
    cronAuthKey :: Maybe CronAuthKey,
    encService :: (String, Word16),
    fcmJsonPath :: Maybe Text,
    exotelCfg :: Maybe ExotelCfg,
    migrationPath :: Maybe FilePath,
    autoMigrate :: Bool,
    coreVersion :: Text,
    domainVersion :: Text,
    loggerConfig :: LoggerConfig,
    geofencingConfig :: GeofencingConfig,
    traceFlag :: TraceFlag,
    signatureExpiry :: NominalDiffTime
  }
  deriving (Generic, FromDhall)

type Env = EnvR AppEnv

type Flow = FlowR AppEnv

type FlowHandler = FlowHandlerR AppEnv

type FlowServer api = FlowServerR AppEnv api
