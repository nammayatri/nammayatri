module App.Types
  ( AppCfg (),
    AppEnv (..),
    Env,
    Flow,
    FlowHandler,
    FlowServer,
    mkAppEnv,
  )
where

import Beckn.Storage.DB.Config
import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Types.Credentials
import Beckn.Utils.Dhall (FromDhall)
import Beckn.Utils.Servant.SignatureAuth
import Data.Time
import EulerHS.Prelude

data AppCfg = AppCfg
  { dbCfg :: DBConfig,
    port :: Int,
    metricsPort :: Int,
    xGatewayUri :: BaseUrl,
    selfId :: Text,
    nwAddress :: BaseUrl,
    migrationPath :: Maybe FilePath,
    autoMigrate :: Bool,
    loggerConfig :: LoggerConfig,
    credRegistry :: [Credential],
    signingKeys :: [SigningKey],
    signatureExpiry :: NominalDiffTime
  }
  deriving (Generic, FromDhall)

data AppEnv = AppEnv
  { dbCfg :: DBConfig,
    xGatewayUri :: BaseUrl,
    selfId :: Text,
    nwAddress :: BaseUrl,
    credRegistry :: [Credential],
    signingKeys :: [SigningKey],
    signatureExpiry :: NominalDiffTime
  }
  deriving (Generic)

mkAppEnv :: AppCfg -> AppEnv
mkAppEnv AppCfg {..} =
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
