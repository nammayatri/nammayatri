module App.Types where

import Beckn.Prelude
import Beckn.Storage.Esqueleto.Config
import Beckn.Types.Common
import Beckn.Utils.App (getPodName)
import Beckn.Utils.Dhall (FromDhall)
import Beckn.Utils.IOLogging
import Beckn.Utils.Servant.Client (HttpClientOptions (..))
import Beckn.Utils.Servant.SignatureAuth
import Beckn.Utils.Shutdown
import Tools.Metrics

data AppCfg = AppCfg
  { port :: Int,
    esqDBCfg :: EsqDBConfig,
    loggerConfig :: LoggerConfig,
    graceTerminationPeriod :: Seconds,
    selfId :: Text,
    httpClientOptions :: HttpClientOptions,
    authEntity :: AuthenticatingEntity',
    authServiceUrl :: BaseUrl
  }
  deriving (Generic, FromDhall)

data AppEnv = AppEnv
  { config :: AppCfg,
    esqDBEnv :: EsqDBEnv,
    isShuttingDown :: Shutdown,
    loggerEnv :: LoggerEnv,
    coreMetrics :: CoreMetricsContainer
  }
  deriving (Generic)

buildAppEnv :: AppCfg -> IO AppEnv
buildAppEnv config@AppCfg {..} = do
  podName <- getPodName
  loggerEnv <- prepareLoggerEnv loggerConfig podName
  esqDBEnv <- prepareEsqDBEnv esqDBCfg loggerEnv
  coreMetrics <- registerCoreMetricsContainer
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
