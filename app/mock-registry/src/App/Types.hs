module App.Types
  ( Env,
    FlowHandler,
    FlowServer,
    AppCfg (),
    AppEnv (..),
    buildAppEnv,
    releaseAppEnv,
  )
where

import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Types.Credentials
import Beckn.Types.Monitoring.Prometheus.Metrics (CoreMetricsContainer, registerCoreMetricsContainer)
import Beckn.Utils.App (getPodName)
import Beckn.Utils.Dhall (FromDhall)
import Beckn.Utils.IOLogging
import Beckn.Utils.Shutdown
import EulerHS.Prelude

data AppCfg = AppCfg
  { port :: Int,
    credRegistry :: [Credential],
    signingKeys :: [SigningKey],
    signatureExpiry :: Seconds,
    graceTerminationPeriod :: Seconds,
    loggerConfig :: LoggerConfig
  }
  deriving (Generic, FromDhall)

data AppEnv = AppEnv
  { config :: AppCfg,
    credRegistry :: [Credential],
    signingKeys :: [SigningKey],
    signatureExpiry :: Seconds,
    isShuttingDown :: TMVar (),
    coreMetrics :: CoreMetricsContainer,
    loggerEnv :: LoggerEnv
  }
  deriving (Generic)

buildAppEnv :: AppCfg -> IO AppEnv
buildAppEnv config@AppCfg {..} = do
  isShuttingDown <- mkShutdown
  coreMetrics <- registerCoreMetricsContainer
  hostname <- getPodName
  loggerEnv <- prepareLoggerEnv loggerConfig hostname
  return AppEnv {..}

releaseAppEnv :: AppEnv -> IO ()
releaseAppEnv AppEnv {..} =
  releaseLoggerEnv loggerEnv

type Env = EnvR AppEnv

type FlowHandler = FlowHandlerR AppEnv

type FlowServer api = FlowServerR AppEnv api
