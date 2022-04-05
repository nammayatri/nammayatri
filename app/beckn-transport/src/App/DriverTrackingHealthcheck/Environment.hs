module App.DriverTrackingHealthcheck.Environment where

import App.DriverTrackingHealthcheck.Config
import Beckn.External.Encryption (EncTools)
import Beckn.Sms.Config (SmsConfig)
import Beckn.Storage.Esqueleto.Config
import Beckn.Types.Common
import Beckn.Utils.App (getPodName)
import Beckn.Utils.IOLogging
import Beckn.Utils.Servant.Client (HttpClientOptions)
import Beckn.Utils.Shutdown
import EulerHS.Prelude
import Tools.Metrics

data AppEnv = AppEnv
  { loggerConfig :: LoggerConfig,
    httpClientOptions :: HttpClientOptions,
    graceTerminationPeriod :: Seconds,
    nwAddress :: BaseUrl,
    fcmJsonPath :: Maybe Text,
    fcmUrl :: BaseUrl,
    encTools :: EncTools,
    driverAllowedDelay :: Seconds,
    notificationMinDelay :: Microseconds,
    driverInactiveDelay :: Seconds,
    smsCfg :: SmsConfig,
    driverInactiveSmsTemplate :: Text,
    esqDBEnv :: EsqDBEnv,
    isShuttingDown :: Shutdown,
    coreMetrics :: CoreMetricsContainer,
    loggerEnv :: LoggerEnv
  }
  deriving (Generic)

buildAppEnv :: AppCfg -> IO AppEnv
buildAppEnv AppCfg {..} = do
  isShuttingDown <- mkShutdown
  hostname <- getPodName
  coreMetrics <- registerCoreMetricsContainer
  loggerEnv <- prepareLoggerEnv loggerConfig hostname
  esqDBEnv <- prepareEsqDBEnv esqDBCfg loggerEnv
  pure AppEnv {..}

releaseAppEnv :: AppEnv -> IO ()
releaseAppEnv AppEnv {..} = do
  releaseLoggerEnv loggerEnv
