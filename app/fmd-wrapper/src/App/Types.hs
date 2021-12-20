module App.Types
  ( AppCfg (),
    AppEnv (..),
    Env,
    FlowHandler,
    FlowServer,
    buildAppEnv,
    releaseAppEnv,
  )
where

import Beckn.Storage.DB.Config (DBConfig)
import Beckn.Storage.Esqueleto.Config
import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Types.Credentials
import Beckn.Utils.Dhall (FromDhall)
import Beckn.Utils.IOLogging
import Beckn.Utils.Servant.Client (HttpClientOptions)
import Beckn.Utils.Servant.SignatureAuth
import qualified Data.Text as T
import EulerHS.Prelude
import qualified EulerHS.Types as T
import System.Environment (lookupEnv)
import Types.Metrics
import Types.Wrapper (DunzoConfig)

data AppCfg = AppCfg
  { dbCfg :: DBConfig,
    esqDBCfg :: EsqDBConfig,
    redisCfg :: T.RedisConfig,
    port :: Int,
    metricsPort :: Int,
    hostName :: Text,
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
    registrySecrets :: RegistrySecrets,
    disableSignatureAuth :: Bool
  }
  deriving (Generic, FromDhall)

data AppEnv = AppEnv
  { config :: AppCfg,
    dbCfg :: DBConfig,
    esqDBEnv :: EsqDBEnv,
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
    registrySecrets :: RegistrySecrets,
    loggerEnv :: LoggerEnv
  }
  deriving (Generic)

buildAppEnv :: AppCfg -> IO AppEnv
buildAppEnv config@AppCfg {..} = do
  hostname <- map T.pack <$> lookupEnv "POD_NAME"
  isShuttingDown <- newEmptyTMVarIO
  coreMetrics <- registerCoreMetricsContainer
  loggerEnv <- prepareLoggerEnv loggerConfig hostname
  esqDBEnv <- prepareEsqDBEnv esqDBCfg loggerEnv
  return $
    AppEnv
      { ..
      }

releaseAppEnv :: AppEnv -> IO ()
releaseAppEnv AppEnv {..} =
  releaseLoggerEnv loggerEnv

type Env = EnvR AppEnv

type FlowHandler = FlowHandlerR AppEnv

type FlowServer api = FlowServerR AppEnv api

instance AuthenticatingEntity AppEnv where
  getRegistry = credRegistry
  getSigningKeys = signingKeys
  getSignatureExpiry = signatureExpiry
