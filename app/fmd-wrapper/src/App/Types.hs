module App.Types
  ( AppCfg (),
    AppEnv (..),
    Env,
    FlowHandler,
    FlowServer,
    buildAppEnv,
  )
where

import Beckn.Storage.DB.Config (DBConfig)
import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Types.Credentials
import Beckn.Types.Flow
import Beckn.Utils.Dhall (FromDhall)
import Beckn.Utils.Servant.Client (HttpClientOptions)
import Beckn.Utils.Servant.SignatureAuth
import EulerHS.Prelude
import qualified EulerHS.Types as T
import Types.Metrics
import Types.Wrapper (DunzoConfig)
import Utils.Auth

data AppCfg = AppCfg
  { dbCfg :: DBConfig,
    redisCfg :: T.RedisConfig,
    port :: Int,
    metricsPort :: Int,
    selfId :: Text,
    xGatewayUri :: BaseUrl,
    xGatewayApiKey :: Maybe Text,
    migrationPath :: Maybe FilePath,
    autoMigrate :: Bool,
    loggerConfig :: LoggerConfig,
    coreVersion :: Text,
    dzConfig :: DunzoConfig,
    credRegistry :: [Credential],
    signingKeys :: [SigningKey],
    signatureExpiry :: Seconds,
    graceTerminationPeriod :: Seconds,
    httpClientOptions :: HttpClientOptions,
    nwAddress :: BaseUrl,
    registryUrl :: BaseUrl,
    registrySecrets :: RegistrySecrets
  }
  deriving (Generic, FromDhall)

data AppEnv = AppEnv
  { dbCfg :: DBConfig,
    selfId :: Text,
    xGatewayUri :: BaseUrl,
    xGatewayApiKey :: Maybe Text,
    coreVersion :: Text,
    dzConfig :: DunzoConfig,
    credRegistry :: [Credential],
    signingKeys :: [SigningKey],
    signatureExpiry :: Seconds,
    isShuttingDown :: TMVar (),
    coreMetrics :: CoreMetricsContainer,
    httpClientOptions :: HttpClientOptions,
    nwAddress :: BaseUrl,
    registryUrl :: BaseUrl,
    registrySecrets :: RegistrySecrets
  }
  deriving (Generic)

instance HasLookupAction LookupRegistryOrg (FlowR AppEnv) where
  runLookup = lookup

instance HasLookupAction LookupRegistryOnSubscribe (FlowR AppEnv) where
  runLookup = lookupAndGetEncPubKey

buildAppEnv :: AppCfg -> IO AppEnv
buildAppEnv AppCfg {..} = do
  isShuttingDown <- newEmptyTMVarIO
  coreMetrics <- registerCoreMetricsContainer
  return $
    AppEnv
      { ..
      }

type Env = EnvR AppEnv

type FlowHandler = FlowHandlerR AppEnv

type FlowServer api = FlowServerR AppEnv api

instance AuthenticatingEntity AppEnv where
  getSelfUrl = xGatewayUri
  getRegistry = credRegistry
  getSigningKeys = signingKeys
  getSignatureExpiry = signatureExpiry
