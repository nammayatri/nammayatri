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
    redisCfg :: T.RedisConfig,
    gwId :: Text,
    gwNwAddress :: BaseUrl,
    credRegistry :: [Credential],
    signingKeys :: [SigningKey],
    cache :: C.Cache Text Text,
    migrationPath :: Maybe FilePath,
    autoMigrate :: Bool,
    loggerConfig :: LoggerConfig,
    searchTimeout :: Maybe Int, -- In seconds
    traceFlag :: TraceFlag,
    mobilityCoreVersion :: Text,
    mobilityDomainVersion :: Text,
    fmdCoreVersion :: Text,
    fmdDomainVersion :: Text,
    signatureExpiry :: NominalDiffTime,
    logContext :: [Text]
  }
  deriving (Generic)

mkAppEnv :: AppCfg -> C.Cache Text Text -> AppEnv
mkAppEnv AppCfg {..} c =
  AppEnv
    { gwId = selfId,
      gwNwAddress = nwAddress,
      cache = c,
      logContext = [],
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

instance HasLogContext AppCfg where
  getLogContext = const []
  setLogContext _ = id

instance HasLogContext AppEnv where
  getLogContext = logContext
  setLogContext ctx env = env {logContext = ctx}
