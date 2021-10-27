module App.Types where

import Beckn.Storage.DB.Config (DBConfig)
import Beckn.Types.App
import Beckn.Types.Common hiding (id)
import Beckn.Types.Credentials
import Beckn.Types.Flow
import Beckn.Utils.Dhall (FromDhall)
import Beckn.Utils.Servant.Client (HttpClientOptions)
import qualified Beckn.Utils.Servant.RegistryService as RegistryService
import Beckn.Utils.Servant.SignatureAuth
import qualified Data.Cache as C
import EulerHS.Prelude
import qualified EulerHS.Types as T
import qualified Storage.Queries.Organization as Org
import Types.Metrics
import Types.Storage.Organization (Organization)

data AppCfg = AppCfg
  { dbCfg :: DBConfig,
    redisCfg :: T.RedisConfig,
    port :: Int,
    metricsPort :: Int,
    selfId :: Text,
    hostName :: Text,
    nwAddress :: BaseUrl,
    credRegistry :: [Credential],
    signingKeys :: [SigningKey],
    migrationPath :: Maybe FilePath,
    autoMigrate :: Bool,
    loggerConfig :: LoggerConfig,
    searchTimeout :: Maybe Seconds,
    coreVersions :: CoreVersions,
    mobilityDomainVersion :: Text,
    signatureExpiry :: Seconds,
    graceTerminationPeriod :: Seconds,
    httpClientOptions :: HttpClientOptions,
    registryUrl :: BaseUrl,
    registrySecrets :: RegistrySecrets
  }
  deriving (Generic, FromDhall)

data AppEnv = AppEnv
  { dbCfg :: DBConfig,
    hostName :: Text,
    gwId :: Text,
    nwAddress :: BaseUrl,
    credRegistry :: [Credential],
    signingKeys :: [SigningKey],
    cache :: C.Cache Text Text,
    coreVersions :: CoreVersions,
    mobilityDomainVersion :: Text,
    signatureExpiry :: Seconds,
    isShuttingDown :: TMVar (),
    coreMetrics :: CoreMetricsContainer,
    httpClientOptions :: HttpClientOptions,
    registryUrl :: BaseUrl,
    registrySecrets :: RegistrySecrets
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
buildAppEnv AppCfg {..} = do
  cache <- C.newCache Nothing
  isShuttingDown <- newEmptyTMVarIO
  coreMetrics <- registerCoreMetricsContainer
  return $
    AppEnv
      { gwId = selfId,
        ..
      }

type FlowHandler = FlowHandlerR AppEnv

type FlowServer r api = FlowServerR AppEnv api

instance AuthenticatingEntity AppEnv where
  getRegistry = credRegistry
  getSigningKeys = signingKeys
  getSignatureExpiry = signatureExpiry

instance HasLookupAction (LookupRegistry Organization) (FlowR AppEnv) where
  runLookup = RegistryService.decodeViaRegistry Org.findOrgByShortId
