module App.Types where

import Beckn.Storage.DB.Config (DBConfig)
import Beckn.Types.App
import Beckn.Types.Common hiding (id)
import Beckn.Types.Credentials
import Beckn.Utils.Dhall (FromDhall)
import Beckn.Utils.Monitoring.Prometheus.Metrics
import Beckn.Utils.Servant.SignatureAuth
import qualified Data.Cache as C
import Data.Time (NominalDiffTime)
import EulerHS.Prelude
import qualified EulerHS.Types as T
import qualified Prometheus as P

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
    traceFlag :: TraceFlag,
    mobilityCoreVersion :: Text,
    mobilityDomainVersion :: Text,
    fmdCoreVersion :: Text,
    fmdDomainVersion :: Text,
    signatureExpiry :: NominalDiffTime
  }
  deriving (Generic, FromDhall)

data AppEnv = AppEnv
  { dbCfg :: DBConfig,
    gwId :: Text,
    gwNwAddress :: BaseUrl,
    credRegistry :: [Credential],
    signingKeys :: [SigningKey],
    cache :: C.Cache Text Text,
    traceFlag :: TraceFlag,
    mobilityCoreVersion :: Text,
    mobilityDomainVersion :: Text,
    fmdCoreVersion :: Text,
    fmdDomainVersion :: Text,
    signatureExpiry :: NominalDiffTime,
    metricsRequestLatencyHistogram :: P.Vector P.Label3 P.Histogram
  }
  deriving (Generic)

mkAppEnv :: AppCfg -> C.Cache Text Text -> IO AppEnv
mkAppEnv AppCfg {..} c = do
  metricsRequestLatencyHistogram <- registerRequestLatencyHistogram
  return $
    AppEnv
      { gwId = selfId,
        gwNwAddress = nwAddress,
        cache = c,
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

instance HasCoreMetrics Flow where
  getRequestLatencyHistogram = metricsRequestLatencyHistogram <$> ask
