{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Environment where

import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.App
import Kernel.Types.Common hiding (id)
import Kernel.Types.Flow
import Kernel.Types.Registry
import Kernel.Utils.App
import Kernel.Utils.Dhall (FromDhall)
import Kernel.Utils.IOLogging
import qualified Kernel.Utils.Registry as Registry
import Kernel.Utils.Servant.Client
import Kernel.Utils.Servant.SignatureAuth
import Kernel.Utils.Shutdown

data AppCfg = AppCfg
  { port :: Int,
    loggerConfig :: LoggerConfig,
    graceTerminationPeriod :: Seconds,
    authEntity :: AuthenticatingEntity',
    registryUrl :: BaseUrl,
    selfId :: Text,
    hostName :: Text,
    httpClientOptions :: HttpClientOptions,
    shortDurationRetryCfg :: RetryCfg,
    longDurationRetryCfg :: RetryCfg,
    disableSignatureAuth :: Bool,
    criticalAPIs :: Metrics.ApiPriorityList
  }
  deriving (Generic, FromDhall)

type MobileNumber = Text

data AppEnv = AppEnv
  { port :: Int,
    loggerConfig :: LoggerConfig,
    selfId :: Text,
    authEntity :: AuthenticatingEntity',
    graceTerminationPeriod :: Seconds,
    registryUrl :: BaseUrl,
    isShuttingDown :: Shutdown,
    loggerEnv :: LoggerEnv,
    coreMetrics :: Metrics.CoreMetricsContainer,
    httpClientOptions :: HttpClientOptions,
    shortDurationRetryCfg :: RetryCfg,
    longDurationRetryCfg :: RetryCfg,
    hostName :: Text,
    disableSignatureAuth :: Bool,
    version :: Metrics.DeploymentVersion,
    criticalAPIs :: Metrics.ApiPriorityList
  }
  deriving (Generic)

buildAppEnv :: AppCfg -> IO AppEnv
buildAppEnv AppCfg {..} = do
  podName <- getPodName
  version <- lookupDeploymentVersion
  loggerEnv <- prepareLoggerEnv loggerConfig podName
  coreMetrics <- registerCoreMetricsContainer
  isShuttingDown <- mkShutdown
  return $ AppEnv {..}

releaseAppEnv :: AppEnv -> IO ()
releaseAppEnv AppEnv {..} =
  releaseLoggerEnv loggerEnv

type FlowHandler = FlowHandlerR AppEnv

type FlowServer api = FlowServerR AppEnv api

type Flow = FlowR AppEnv

instance AuthenticatingEntity AppEnv where
  getSigningKey = (.authEntity.signingKey)
  getSignatureExpiry = (.authEntity.signatureExpiry)

instance Registry Flow where
  registryLookup sReq = do
    registryUrl <- asks (.registryUrl)
    Registry.registryLookup registryUrl sReq
