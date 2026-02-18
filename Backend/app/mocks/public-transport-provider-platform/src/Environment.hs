{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Environment where

import Control.Exception (try)
import Control.Monad.Catch (bracket)
import Kernel.Mock.ExternalAPI
import Kernel.Storage.Hedis
import Kernel.Streaming.Kafka.Producer.Types (KafkaProducerTools)
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.Common
import Kernel.Utils.Dhall (FromDhall)
import Kernel.Utils.IOLogging
import Kernel.Utils.Servant.SignatureAuth hiding (prepareAuthManager)
import Network.HTTP.Client (Manager, newManager)
import Relude
import System.Environment as SE

data AppCfg = AppCfg
  { port :: Int,
    selfId :: Text,
    uniqueKeyId :: Text,
    selfUri :: BaseUrl,
    hedisCfg :: HedisCfg,
    hedisClusterCfg :: HedisCfg,
    hedisMigrationStage :: Bool,
    hedisNonCriticalCfg :: HedisCfg,
    hedisNonCriticalClusterCfg :: HedisCfg,
    hedisSecondaryClusterCfg :: HedisCfg,
    cutOffHedisCluster :: Bool,
    statusWaitTimeSec :: Seconds,
    callbackWaitTimeMilliSec :: Milliseconds,
    loggerConfig :: LoggerConfig,
    authEntity :: AuthenticatingEntity',
    enableRedisLatencyLogging :: Bool,
    enablePrometheusMetricLogging :: Bool
  }
  deriving (Generic, FromDhall)

data AppEnv = AppEnv
  { selfId :: Text,
    uniqueKeyId :: Text,
    selfUri :: BaseUrl,
    statusWaitTimeSec :: Seconds,
    callbackWaitTimeMilliSec :: Milliseconds,
    loggerConfig :: LoggerConfig,
    coreMetrics :: CoreMetricsContainer,
    authEntity :: AuthenticatingEntity',
    hedisEnv :: HedisEnv,
    hedisNonCriticalEnv :: HedisEnv,
    hedisNonCriticalClusterEnv :: HedisEnv,
    hedisClusterEnv :: HedisEnv,
    secondaryHedisClusterEnv :: Maybe HedisEnv,
    hedisMigrationStage :: Bool,
    cutOffHedisCluster :: Bool,
    loggerEnv :: LoggerEnv,
    authManager :: Manager,
    enableRedisLatencyLogging :: Bool,
    enablePrometheusMetricLogging :: Bool,
    shouldLogRequestId :: Bool,
    sessionId :: Maybe Text,
txnId :: Maybe Text,
    requestId :: Maybe Text,
    kafkaProducerForART :: Maybe KafkaProducerTools,
    url :: Maybe Text
  }
  deriving (Generic)

buildAppEnv :: AppCfg -> IO AppEnv
buildAppEnv config@AppCfg {..} = do
  hedisEnv <- connectHedis hedisCfg ("mock_public_transport_provider_platform" <>)
  hedisNonCriticalEnv <- connectHedis hedisNonCriticalCfg ("mock_public_transport_provider_platform" <>)
  let requestId = Nothing
  let sessionId = Nothing
      txnId = Nothing
  shouldLogRequestId <- fromMaybe False . (>>= readMaybe) <$> SE.lookupEnv "SHOULD_LOG_REQUEST_ID"
  let kafkaProducerForART = Nothing
  hedisNonCriticalClusterEnv <-
    if cutOffHedisCluster
      then pure hedisNonCriticalEnv
      else connectHedisCluster hedisNonCriticalClusterCfg ("mock_public_transport_provider_platform" <>)
  coreMetrics <- registerCoreMetricsContainer
  hedisClusterEnv <-
    if cutOffHedisCluster
      then pure hedisEnv
      else connectHedisCluster hedisClusterCfg ("mock_public_transport_provider_platform" <>)
  secondaryHedisClusterEnv <-
    try (connectHedisCluster hedisSecondaryClusterCfg ("mock_public_transport_provider_platform" <>)) >>= \case
      Left (e :: SomeException) -> do
        putStrLn $ "ERROR: Failed to connect to secondary hedis cluster: " ++ show e
        pure Nothing
      Right env -> pure (Just env)
  loggerEnv <- prepareLoggerEnv loggerConfig Nothing
  let authManagerSettings = prepareAuthManager config ["Authorization"] selfId uniqueKeyId (\lvl msg -> logOutputIO loggerEnv lvl msg requestId sessionId)
  authManager <- newManager authManagerSettings
  let url = Nothing
  return $ AppEnv {..}

releaseAppEnv :: AppEnv -> IO ()
releaseAppEnv AppEnv {..} = do
  disconnectHedis hedisEnv
  disconnectHedis hedisClusterEnv
  maybe (pure ()) disconnectHedis secondaryHedisClusterEnv
  releaseLoggerEnv loggerEnv

withAppEnv :: AppCfg -> (AppEnv -> IO ()) -> IO ()
withAppEnv cfg = bracket (buildAppEnv cfg) releaseAppEnv

instance AuthenticatingEntity AppCfg where
  getSigningKey = (.authEntity.signingKey)
  getSignatureExpiry = (.authEntity.signatureExpiry)
