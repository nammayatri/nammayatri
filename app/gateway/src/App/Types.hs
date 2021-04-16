module App.Types where

import Beckn.Storage.DB.Config (DBConfig)
import Beckn.Types.App
import Beckn.Types.Common hiding (id)
import Beckn.Types.Credentials
import Beckn.Utils.Dhall (FromDhall)
import Beckn.Utils.Servant.SignatureAuth
import qualified Data.Cache as C
import Data.Time (NominalDiffTime)
import EulerHS.Prelude
import qualified EulerHS.Types as T
import Types.Metrics

data AppCfg = AppCfg
  { dbCfg :: DBConfig,
    redisCfg :: T.RedisConfig,
    port :: Int,
    metricsPort :: Int,
    selfId :: Text,
    nwAddress :: BaseUrl,
    credRegistry :: [Credential],
    signingKeys :: [SigningKey],
    migrationPath :: Maybe FilePath,
    autoMigrate :: Bool,
    loggerConfig :: LoggerConfig,
    searchTimeout :: Maybe Int,
    mobilityCoreVersion :: Text,
    mobilityDomainVersion :: Text,
    fmdCoreVersion :: Text,
    fmdDomainVersion :: Text,
    signatureExpiry :: NominalDiffTime,
    graceTerminationPeriod :: Int
  }
  deriving (Generic, FromDhall)

data AppEnv = AppEnv
  { dbCfg :: DBConfig,
    gwId :: Text,
    gwNwAddress :: BaseUrl,
    credRegistry :: [Credential],
    signingKeys :: [SigningKey],
    cache :: C.Cache Text Text,
    mobilityCoreVersion :: Text,
    mobilityDomainVersion :: Text,
    fmdCoreVersion :: Text,
    fmdDomainVersion :: Text,
    signatureExpiry :: NominalDiffTime,
    isShuttingDown :: TMVar (),
    metricsRequestLatency :: RequestLatencyMetric
  }
  deriving (Generic)

buildAppEnv :: AppCfg -> IO AppEnv
buildAppEnv AppCfg {..} = do
  cache <- C.newCache Nothing
  isShuttingDown <- newEmptyTMVarIO
  metricsRequestLatency <- registerRequestLatencyMetric
  return $
    AppEnv
      { gwId = selfId,
        gwNwAddress = nwAddress,
        ..
      }

type Flow = FlowR AppEnv

type FlowHandler = FlowHandlerR AppEnv

type FlowServer r api = FlowServerR AppEnv api

instance AuthenticatingEntity AppEnv where
  getSelfId = gwId
  getSelfUrl = gwNwAddress
  getRegistry = credRegistry
  getSigningKeys = signingKeys
  getSignatureExpiry = signatureExpiry
