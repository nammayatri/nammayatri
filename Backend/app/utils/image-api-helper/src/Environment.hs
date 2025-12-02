{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Environment where

import Control.Monad.Catch (bracket)
import Kernel.Tools.Metrics.CoreMetrics.Types
import Kernel.Types.Common
import Kernel.Utils.App (lookupDeploymentVersion)
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
    coreMetrics :: CoreMetricsContainer,
    version :: DeploymentVersion,
    requestId :: Maybe Text,
    url :: Maybe Text
  }
  deriving (Generic)

buildAppEnv :: AppCfg -> IO AppEnv
buildAppEnv AppCfg {..} = do
  version <- lookupDeploymentVersion
  loggerEnv <- prepareLoggerEnv loggerConfig Nothing
  coreMetrics <- registerCoreMetricsContainer
  let requestId = Nothing
  let url = Nothing
  return $ AppEnv {..}

releaseAppEnv :: AppEnv -> IO ()
releaseAppEnv AppEnv {..} = do
  releaseLoggerEnv loggerEnv

withAppEnv :: AppCfg -> (AppEnv -> IO ()) -> IO ()
withAppEnv cfg = bracket (buildAppEnv cfg) releaseAppEnv
