module App.DriverTrackingHealthcheck.Environment where

import App.DriverTrackingHealthcheck.Config
import Beckn.External.Encryption (EncTools)
import Beckn.Storage.Esqueleto.Config
import Beckn.Types.Common
import Beckn.Utils.App (getPodName)
import Beckn.Utils.IOLogging
import Beckn.Utils.Shutdown
import EulerHS.Prelude
import Tools.Metrics

data AppEnv = AppEnv
  { config :: AppCfg,
    esqDBEnv :: EsqDBEnv,
    fcmJsonPath :: Maybe Text,
    fcmUrl :: BaseUrl,
    nwAddress :: BaseUrl,
    isShuttingDown :: Shutdown,
    coreMetrics :: CoreMetricsContainer,
    loggerEnv :: LoggerEnv,
    encTools :: EncTools
  }
  deriving (Generic)

buildAppEnv :: AppCfg -> IO AppEnv
buildAppEnv config@AppCfg {..} = do
  isShuttingDown <- mkShutdown
  hostname <- getPodName
  coreMetrics <- registerCoreMetricsContainer
  loggerEnv <- prepareLoggerEnv loggerConfig hostname
  esqDBEnv <- prepareEsqDBEnv esqDBCfg loggerEnv
  pure AppEnv {..}

releaseAppEnv :: AppEnv -> IO ()
releaseAppEnv AppEnv {..} = do
  releaseLoggerEnv loggerEnv
