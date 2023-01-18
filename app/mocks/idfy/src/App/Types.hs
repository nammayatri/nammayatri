module App.Types where

import Beckn.Tools.Metrics.CoreMetrics
import Beckn.Types.App
import Beckn.Types.Common hiding (id)
import Beckn.Types.Flow
import Beckn.Utils.Dhall
import Beckn.Utils.IOLogging
import Beckn.Utils.Servant.Client (HttpClientOptions, RetryCfg)
import Beckn.Utils.Shutdown
import qualified Data.Text as T
import EulerHS.Prelude
import System.Environment (lookupEnv)
import Types.Common

data AppCfg = AppCfg
  { port :: Int,
    loggerConfig :: LoggerConfig,
    callbackWaitTimeMilliSec :: Milliseconds,
    httpClientOptions :: HttpClientOptions,
    shortDurationRetryCfg :: RetryCfg,
    longDurationRetryCfg :: RetryCfg,
    graceTerminationPeriod :: Seconds,
    webhookUrl :: BaseUrl,
    secret :: Text,
    accountId :: AccountId,
    apiKey :: ApiKey
  }
  deriving (Generic)

data AppEnv = AppEnv
  { port :: Int,
    loggerConfig :: LoggerConfig,
    loggerEnv :: LoggerEnv,
    callbackWaitTimeMilliSec :: Milliseconds,
    graceTerminationPeriod :: Seconds,
    coreMetrics :: CoreMetricsContainer,
    isShuttingDown :: Shutdown,
    webhookUrl :: BaseUrl,
    secret :: Text,
    httpClientOptions :: HttpClientOptions,
    shortDurationRetryCfg :: RetryCfg,
    longDurationRetryCfg :: RetryCfg,
    accountId :: AccountId,
    apiKey :: ApiKey
  }
  deriving (Generic)

buildAppEnv :: AppCfg -> IO AppEnv
buildAppEnv AppCfg {..} = do
  hostname <- map T.pack <$> lookupEnv "POD_NAME"
  loggerEnv <- prepareLoggerEnv loggerConfig hostname
  coreMetrics <- registerCoreMetricsContainer
  isShuttingDown <- mkShutdown
  return $ AppEnv {..}

releaseAppEnv :: AppEnv -> IO ()
releaseAppEnv AppEnv {..} =
  releaseLoggerEnv loggerEnv

type FlowHandler = FlowHandlerR AppEnv

type FlowServer api = FlowServerR AppEnv api

type Flow = FlowR AppEnv
