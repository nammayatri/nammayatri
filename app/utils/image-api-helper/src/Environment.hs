{-# OPTIONS_GHC -Wno-orphans #-}

module Environment where

import Control.Monad.Catch (bracket)
import Kernel.Tools.Metrics.CoreMetrics.Types
import Kernel.Types.Common
import Kernel.Utils.Dhall (FromDhall)
import Kernel.Utils.IOLogging
import Relude

data AppCfg = AppCfg
  { port :: Int,
    loggerConfig :: LoggerConfig
  }
  deriving (Generic, FromDhall)

data AppEnv = AppEnv
  { loggerConfig :: LoggerConfig,
    loggerEnv :: LoggerEnv,
    coreMetrics :: CoreMetricsContainer
  }
  deriving (Generic)

buildAppEnv :: AppCfg -> IO AppEnv
buildAppEnv AppCfg {..} = do
  loggerEnv <- prepareLoggerEnv loggerConfig Nothing
  coreMetrics <- registerCoreMetricsContainer
  return $ AppEnv {..}

releaseAppEnv :: AppEnv -> IO ()
releaseAppEnv AppEnv {..} = do
  releaseLoggerEnv loggerEnv

withAppEnv :: AppCfg -> (AppEnv -> IO ()) -> IO ()
withAppEnv cfg = bracket (buildAppEnv cfg) releaseAppEnv
