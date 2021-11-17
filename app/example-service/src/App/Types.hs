module App.Types where

import Beckn.Prelude
import Beckn.Storage.Esqueleto.Config
import Beckn.Types.Common
import Beckn.Utils.Dhall (FromDhall)
import Beckn.Utils.Shutdown
import qualified Data.Text as T
import System.Environment (lookupEnv)

data AppCfg = AppCfg
  { esqDBCfg :: EsqDBConfig,
    port :: Int,
    loggerConfig :: LoggerConfig,
    graceTerminationPeriod :: Seconds
  }
  deriving (Generic, FromDhall)

type MobileNumber = Text

data AppEnv = AppEnv
  { config :: AppCfg,
    esqDBEnv :: EsqDBEnv,
    isShuttingDown :: Shutdown,
    loggerEnv :: LoggerEnv
  }
  deriving (Generic)

buildAppEnv :: AppCfg -> IO AppEnv
buildAppEnv config@AppCfg {..} = do
  hostname <- fmap T.pack <$> lookupEnv "POD_NAME"
  loggerEnv <- prepareLoggerEnv loggerConfig hostname
  esqDBEnv <- prepareEsqDBEnv esqDBCfg loggerEnv
  isShuttingDown <- mkShutdown
  return $ AppEnv {..}

releaseAppEnv :: AppEnv -> IO ()
releaseAppEnv AppEnv {..} =
  releaseLoggerEnv loggerEnv

type FlowHandler = FlowHandlerR AppEnv

type FlowServer r api = FlowServerR AppEnv api
