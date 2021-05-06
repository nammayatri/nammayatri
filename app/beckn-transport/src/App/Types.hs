module App.Types
  ( AppCfg (),
    AppEnv (..),
    DriverAllocationConfig (..),
    Env,
    Flow,
    FlowHandler,
    FlowServer,
    Log (..),
    mkAppEnv,
  )
where

import Beckn.External.Exotel.Types (ExotelCfg)
import Beckn.Sms.Config (SmsConfig)
import Beckn.Storage.DB.Config (DBConfig)
import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Types.Credentials
import Beckn.Utils.Dhall (FromDhall)
import Beckn.Utils.Monitoring.Prometheus.Metrics
import Beckn.Utils.Servant.SignatureAuth
import Data.Time (NominalDiffTime)
import EulerHS.Prelude
import qualified EulerHS.Types as T
import qualified Prometheus as P
import Types.App (SortMode)

data AppCfg = AppCfg
  { dbCfg :: DBConfig,
    redisCfg :: T.RedisConfig,
    smsCfg :: SmsConfig,
    otpSmsTemplate :: Text,
    inviteSmsTemplate :: Text,
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
    graphhopperUrl :: BaseUrl
  }
  deriving (Generic, FromDhall)

data AppEnv = AppEnv
  { dbCfg :: DBConfig,
    smsCfg :: SmsConfig,
    otpSmsTemplate :: Text,
    inviteSmsTemplate :: Text,
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
    coreVersion :: Text,
    domainVersion :: Text,
    traceFlag :: TraceFlag,
    signatureExpiry :: NominalDiffTime,
    driverAllocationConfig :: DriverAllocationConfig,
    googleMapsUrl :: BaseUrl,
    googleMapsKey :: Text,
    fcmUrl :: BaseUrl,
    graphhopperUrl :: BaseUrl,
    metricsRequestLatencyHistogram :: P.Vector P.Label3 P.Histogram
  }
  deriving (Generic)

data DriverAllocationConfig = DriverAllocationConfig
  { driverNotificationExpiry :: NominalDiffTime,
    rideAllocationExpiry :: NominalDiffTime,
    defaultSortMode :: SortMode,
    defaultRadiusOfSearch :: Integer,
    requestsNumPerIteration :: Integer,
    processDelay :: NominalDiffTime
  }
  deriving (Generic, FromDhall)

mkAppEnv :: AppCfg -> IO AppEnv
mkAppEnv AppCfg {..} = do
  metricsRequestLatencyHistogram <- registerRequestLatencyHistogram
  return $
    AppEnv
      { ..
      }

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

instance HasCoreMetrics Flow where
  getRequestLatencyHistogram = metricsRequestLatencyHistogram <$> ask
