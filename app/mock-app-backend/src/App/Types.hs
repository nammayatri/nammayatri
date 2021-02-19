module App.Types where

import Beckn.Storage.DB.Config
import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Types.Credentials
import Beckn.Utils.Dhall (FromDhall)
import Beckn.Utils.Logging
import Beckn.Utils.Servant.SignatureAuth
import Data.Time
import EulerHS.Prelude

data AppEnv = AppEnv
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
    signatureExpiry :: NominalDiffTime,
    logContext :: [Text]
  }
  deriving (Generic, FromDhall)

type Env = EnvR AppEnv

type Flow = FlowR AppEnv

type FlowHandler = FlowHandlerR AppEnv

type FlowServer api = FlowServerR AppEnv api

instance AuthenticatingEntity AppEnv where
  getSelfId = selfId
  getSelfUrl = nwAddress
  getRegistry = credRegistry
  getSigningKeys = signingKeys
  getSignatureExpiry = signatureExpiry

instance HasLogContext AppEnv where
  getLogContext = logContext
  setLogContext ctx env = env {logContext = ctx}
