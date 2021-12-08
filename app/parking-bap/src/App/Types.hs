module App.Types where

import Beckn.Prelude
import Beckn.Storage.Esqueleto.Config
import Beckn.Storage.Redis.Config (RedisConfig)
import Beckn.Types.Common
import Beckn.Utils.App (getPodName)
import Beckn.Utils.Dhall (FromDhall)
import Beckn.Utils.IOLogging
import Beckn.Utils.Servant.Client (HttpClientOptions (..))
import Beckn.Utils.Servant.SignatureAuth
import Beckn.Utils.Shutdown
import qualified Tools.Metrics.Types as Metrics

data AppCfg = AppCfg
  { port :: Int,
    esqDBCfg :: EsqDBConfig,
    redisCfg :: RedisConfig,
    loggerConfig :: LoggerConfig,
    graceTerminationPeriod :: Seconds,
    selfId :: Text,
    selfURI :: BaseUrl,
    httpClientOptions :: HttpClientOptions,
    authEntity :: AuthenticatingEntity',
    authServiceUrl :: BaseUrl,
    gatewayUrl :: BaseUrl,
    metricsSearchDurationTimeout :: Seconds,
    coreVersion :: Text,
    domainVersion :: Text,
    hostName :: Text,
    registryUrl :: BaseUrl,
    migrationPath :: Maybe FilePath,
    autoMigrate :: Bool
  }
  deriving (Generic, FromDhall)

data AppEnv = AppEnv
  { config :: AppCfg,
    esqDBEnv :: EsqDBEnv,
    isShuttingDown :: Shutdown,
    loggerEnv :: LoggerEnv,
    coreMetrics :: Metrics.CoreMetricsContainer,
    bapMetrics :: Metrics.BAPMetricsContainer,
    coreVersion :: Text, -- FIXME this two fields are duplicated with AppCfg fields for context validation
    domainVersion :: Text -- FIXME this two fields are duplicated with AppCfg fields for context validation
  }
  deriving (Generic)

buildAppEnv :: AppCfg -> IO AppEnv
buildAppEnv config@AppCfg {..} = do
  podName <- getPodName
  loggerEnv <- prepareLoggerEnv loggerConfig podName
  esqDBEnv <- prepareEsqDBEnv esqDBCfg loggerEnv
  coreMetrics <- Metrics.registerCoreMetricsContainer
  bapMetrics <- Metrics.registerBAPMetricsContainer metricsSearchDurationTimeout
  isShuttingDown <- mkShutdown
  pure $ AppEnv {..}

releaseAppEnv :: AppEnv -> IO ()
releaseAppEnv AppEnv {..} =
  releaseLoggerEnv loggerEnv

type FlowHandler = FlowHandlerR AppEnv

type FlowServer api = FlowServerR AppEnv api

instance AuthenticatingEntity AppEnv where
  getRegistry = (.config.authEntity.credRegistry)
  getSigningKeys = (.config.authEntity.signingKeys)
  getSignatureExpiry = (.config.authEntity.signatureExpiry)
