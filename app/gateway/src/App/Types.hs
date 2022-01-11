module App.Types where

import Beckn.Storage.DB.Config (DBConfig)
import Beckn.Storage.Esqueleto.Config
import Beckn.Types.App
import Beckn.Types.Common hiding (id)
import Beckn.Utils.Dhall (FromDhall)
import Beckn.Utils.IOLogging
import Beckn.Utils.Servant.Client (HttpClientOptions)
import Beckn.Utils.Servant.SignatureAuth
import qualified Data.Cache as C
import qualified Data.Text as T
import EulerHS.Prelude
import qualified EulerHS.Types as T
import System.Environment (lookupEnv)
import Types.Metrics

data AppCfg = AppCfg
  { dbCfg :: DBConfig,
    esqDBCfg :: EsqDBConfig,
    redisCfg :: T.RedisConfig,
    port :: Int,
    metricsPort :: Int,
    selfId :: Text,
    hostName :: Text,
    nwAddress :: BaseUrl,
    authEntity :: AuthenticatingEntity',
    migrationPath :: Maybe FilePath,
    autoMigrate :: Bool,
    loggerConfig :: LoggerConfig,
    searchTimeout :: Maybe Seconds,
    coreVersions :: CoreVersions,
    mobilityDomainVersion :: Text,
    graceTerminationPeriod :: Seconds,
    httpClientOptions :: HttpClientOptions,
    registryUrl :: BaseUrl,
    registrySecrets :: RegistrySecrets,
    disableSignatureAuth :: Bool
  }
  deriving (Generic, FromDhall)

data AppEnv = AppEnv
  { config :: AppCfg,
    dbCfg :: DBConfig,
    esqDBEnv :: EsqDBEnv,
    gwId :: Text,
    nwAddress :: BaseUrl,
    cache :: C.Cache Text Text,
    coreVersions :: CoreVersions,
    mobilityDomainVersion :: Text,
    isShuttingDown :: TMVar (),
    coreMetrics :: CoreMetricsContainer,
    registrySecrets :: RegistrySecrets,
    loggerEnv :: LoggerEnv
  }
  deriving (Generic)

data CoreVersions = CoreVersions
  { mobility :: Text,
    finalMileDelivery :: Text,
    localRetail :: Text,
    foodAndBeverage :: Text
  }
  deriving (Generic, FromDhall)

buildAppEnv :: AppCfg -> IO AppEnv
buildAppEnv config@AppCfg {..} = do
  hostname <- map T.pack <$> lookupEnv "POD_NAME"
  cache <- C.newCache Nothing
  isShuttingDown <- newEmptyTMVarIO
  coreMetrics <- registerCoreMetricsContainer
  loggerEnv <- prepareLoggerEnv loggerConfig hostname
  esqDBEnv <- prepareEsqDBEnv esqDBCfg loggerEnv
  return $
    AppEnv
      { gwId = selfId,
        ..
      }

releaseAppEnv :: AppEnv -> IO ()
releaseAppEnv AppEnv {..} =
  releaseLoggerEnv loggerEnv

type FlowHandler = FlowHandlerR AppEnv

type FlowServer r api = FlowServerR AppEnv api

instance AuthenticatingEntity AppEnv where
  getSigningKey = (.config.authEntity.signingKey)
  getUniqueKeyId = (.config.authEntity.uniqueKeyId)
  getSignatureExpiry = (.config.authEntity.signatureExpiry)
