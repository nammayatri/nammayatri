module App.Types
  ( module App.Types,
    Log (..),
  )
where

import Beckn.External.Exotel.Types (ExotelCfg)
import Beckn.Sms.Config (SmsConfig)
import Beckn.Storage.DB.Config (DBConfig)
import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Types.Credentials
import Beckn.Utils.Dhall (FromDhall)
import Beckn.Utils.Logging (HasLogContext (..), Log (..), LoggerConfig)
import Beckn.Utils.Servant.SignatureAuth
import Data.Time (NominalDiffTime)
import EulerHS.Prelude
import qualified EulerHS.Types as T
import Types.App (SortMode)

data AppEnv = AppEnv
  { dbCfg :: DBConfig,
    redisCfg :: T.RedisConfig,
    smsCfg :: SmsConfig,
    port :: Int,
    bgtmPort :: Int,
    metricsPort :: Int,
    xGatewaySelector :: Maybe Text,
    xGatewayNsdlUrl :: Maybe BaseUrl,
    xAppUri :: BaseUrl,
    selfId :: Text,
    nwAddress :: BaseUrl,
    credRegistry :: [Credential],
    signingKeys :: [SigningKey],
    caseExpiry :: Maybe Integer,
    cronAuthKey :: Maybe CronAuthKey,
    encService :: (String, Word16),
    fcmJsonPath :: Maybe Text,
    exotelCfg :: Maybe ExotelCfg,
    migrationPath :: Maybe FilePath,
    autoMigrate :: Bool,
    coreVersion :: Text,
    domainVersion :: Text,
    loggerConfig :: LoggerConfig,
    traceFlag :: TraceFlag,
    signatureExpiry :: NominalDiffTime,
    driverAllocationConfig :: DriverAllocationConfig,
    googleMapsUrl :: BaseUrl,
    googleMapsKey :: Text,
    fcmUrl :: BaseUrl,
    graphhopperUrl :: BaseUrl,
    logContext :: [Text]
  }
  deriving (Generic, FromDhall)

data DriverAllocationConfig = DriverAllocationConfig
  { driverNotificationExpiry :: NominalDiffTime,
    rideAllocationExpiry :: NominalDiffTime,
    defaultSortMode :: SortMode,
    defaultRadiusOfSearch :: Integer,
    requestsNumPerIteration :: Integer,
    processDelay :: NominalDiffTime
  }
  deriving (Generic, FromDhall)

type Env = EnvR AppEnv

type Flow = FlowR AppEnv

type FlowHandler = FlowHandlerR AppEnv

type FlowServer api = FlowServerR AppEnv api

instance AuthenticatingEntity AppEnv where
  getSelfId = selfId
  getSelfUrl = nwAddress
  getRegistry = credRegistry
  getSigningKeys = signingKeys
  getSignatureExpiry = signatureExpiry

instance HasLogContext AppEnv where
  getLogContext = logContext
  setLogContext ctx env = env {logContext = ctx}
