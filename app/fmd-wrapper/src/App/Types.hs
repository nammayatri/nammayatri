module App.Types
  ( AppCfg (),
    AppEnv (..),
    Env,
    Flow,
    FlowHandler,
    FlowServer,
    buildAppEnv,
  )
where

import Beckn.Storage.DB.Config (DBConfig)
import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Types.Credentials
import Beckn.Utils.Dhall (FromDhall)
import Beckn.Utils.Servant.SignatureAuth
  ( AuthenticatingEntity (..),
  )
import Data.Time
import EulerHS.Prelude
import qualified EulerHS.Types as T
import Types.Metrics
import Types.Wrapper (DunzoConfig)

data AppCfg = AppCfg
  { dbCfg :: DBConfig,
    redisCfg :: T.RedisConfig,
    port :: Int,
    selfId :: Text,
    xGatewayUri :: BaseUrl,
    xGatewayApiKey :: Maybe Text,
    migrationPath :: Maybe FilePath,
    autoMigrate :: Bool,
    loggerConfig :: LoggerConfig,
    coreVersion :: Text,
    domainVersion :: Text,
    dzConfig :: DunzoConfig,
    credRegistry :: [Credential],
    signingKeys :: [SigningKey],
    signatureExpiry :: NominalDiffTime,
    graceTerminationPeriod :: Int
  }
  deriving (Generic, FromDhall)

data AppEnv = AppEnv
  { dbCfg :: DBConfig,
    selfId :: Text,
    xGatewayUri :: BaseUrl,
    xGatewayApiKey :: Maybe Text,
    coreVersion :: Text,
    domainVersion :: Text,
    dzConfig :: DunzoConfig,
    credRegistry :: [Credential],
    signingKeys :: [SigningKey],
    signatureExpiry :: NominalDiffTime,
    isShuttingDown :: TMVar (),
    coreMetrics :: CoreMetricsContainer
  }
  deriving (Generic)

buildAppEnv :: AppCfg -> IO AppEnv
buildAppEnv AppCfg {..} = do
  isShuttingDown <- newEmptyTMVarIO
  coreMetrics <- registerCoreMetricsContainer
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
  getSelfUrl = xGatewayUri
  getRegistry = credRegistry
  getSigningKeys = signingKeys
  getSignatureExpiry = signatureExpiry
