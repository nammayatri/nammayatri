{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Environment
  ( HandlerCfg (..),
    HandlerEnv (..),
    SchedulerConfig (..),
    Flow,
    buildHandlerEnv,
    releaseHandlerEnv,
  )
where

import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import Data.String.Conversions (cs)
import "rider-app" Environment (AppCfg (..))
import Kernel.External.Encryption (EncTools)
import Kernel.Prelude
import Kernel.Sms.Config (SmsConfig)
import Kernel.Storage.Clickhouse.Config
import Kernel.Storage.Esqueleto.Config
import Kernel.Storage.Hedis (HedisEnv, connectHedis, connectHedisCluster, disconnectHedis)
import Kernel.Streaming.Kafka.Producer.Types
import Kernel.Types.Base64 (Base64)
import Kernel.Types.Common
import Kernel.Types.Flow
import Kernel.Utils.App (lookupDeploymentVersion)
import Kernel.Utils.Common
import Kernel.Utils.Dhall (FromDhall)
import Kernel.Utils.IOLogging
import Kernel.Utils.Servant.SignatureAuth
import Lib.Scheduler (SchedulerType)
import Lib.Scheduler.Environment (SchedulerConfig (..))
import Lib.SessionizerMetrics.Prometheus.Internal
import Lib.SessionizerMetrics.Types.Event hiding (id)
import Passetto.Client
import SharedLogic.GoogleTranslate
import System.Environment (lookupEnv)
import Tools.Metrics

data HandlerCfg = HandlerCfg
  { schedulerConfig :: SchedulerConfig,
    appCfg :: AppCfg
  }
  deriving (Generic, FromDhall)

data HandlerEnv = HandlerEnv
  { signingKey :: Base64,
    signatureExpiry :: Seconds,
    httpClientOptions :: HttpClientOptions,
    shortDurationRetryCfg :: RetryCfg,
    longDurationRetryCfg :: RetryCfg,
    loggerEnv :: LoggerEnv,
    esqDBEnv :: EsqDBEnv,
    esqDBReplicaEnv :: EsqDBEnv,
    encTools :: EncTools,
    hedisEnv :: HedisEnv,
    kafkaProducerTools :: KafkaProducerTools,
    hedisNonCriticalEnv :: HedisEnv,
    hedisNonCriticalClusterEnv :: HedisEnv,
    hedisClusterEnv :: HedisEnv,
    cutOffHedisCluster :: Bool,
    hedisMigrationStage :: Bool,
    cacheConfig :: CacheConfig,
    cacheTranslationConfig :: CacheTranslationConfig,
    googleTranslateUrl :: BaseUrl,
    googleTranslateKey :: Text,
    coreMetrics :: CoreMetricsContainer,
    eventRequestCounter :: EventCounterMetric,
    eventStreamMap :: [EventStreamMap],
    maxShards :: Int,
    version :: DeploymentVersion,
    jobInfoMap :: M.Map Text Bool,
    enableRedisLatencyLogging :: Bool,
    enablePrometheusMetricLogging :: Bool,
    schedulerSetName :: Text,
    kvConfigUpdateFrequency :: Int,
    smsCfg :: SmsConfig,
    schedulerType :: SchedulerType,
    requestId :: Maybe Text,
    shouldLogRequestId :: Bool,
    kafkaProducerForART :: Maybe KafkaProducerTools,
    internalEndPointHashMap :: HM.HashMap BaseUrl BaseUrl,
    cacConfig :: CacConfig,
    passettoContext :: PassettoContext,
    serviceClickhouseEnv :: ClickhouseEnv,
    serviceClickhouseCfg :: ClickhouseCfg
  }
  deriving (Generic)

buildHandlerEnv :: HandlerCfg -> IO HandlerEnv
buildHandlerEnv HandlerCfg {..} = do
  let AppCfg {..} = appCfg
  hostname <- fmap cs <$> lookupEnv "POD_NAME" :: IO (Maybe Text)
  version <- lookupDeploymentVersion
  loggerEnv <- prepareLoggerEnv appCfg.loggerConfig hostname
  esqDBEnv <- prepareEsqDBEnv appCfg.esqDBCfg loggerEnv
  esqDBReplicaEnv <- prepareEsqDBEnv appCfg.esqDBReplicaCfg loggerEnv
  kafkaProducerTools <- buildKafkaProducerTools appCfg.kafkaProducerCfg
  eventRequestCounter <- registerEventRequestCounterMetric
  passettoContext <- (uncurry mkDefPassettoContext) encTools.service
  let requestId = Nothing
  shouldLogRequestId <- fromMaybe False . (>>= readMaybe) <$> lookupEnv "SHOULD_LOG_REQUEST_ID"
  let kafkaProducerForART = Just kafkaProducerTools
  hedisEnv <- connectHedis appCfg.hedisCfg ("rider-app-scheduler:" <>)
  hedisNonCriticalEnv <- connectHedis appCfg.hedisNonCriticalCfg ("ras:n_c:" <>)
  hedisClusterEnv <-
    if cutOffHedisCluster
      then pure hedisEnv
      else connectHedisCluster hedisClusterCfg ("rider-app-scheduler:" <>)
  hedisNonCriticalClusterEnv <-
    if cutOffHedisCluster
      then pure hedisNonCriticalEnv
      else connectHedisCluster hedisNonCriticalClusterCfg ("doa:n_c:" <>)
  let jobInfoMap :: (M.Map Text Bool) = M.mapKeys show jobInfoMapx
  let internalEndPointHashMap = HM.fromList $ M.toList internalEndPointMap
  coreMetrics <- registerCoreMetricsContainer
  serviceClickhouseEnv <- createConn riderClickhouseCfg
  let serviceClickhouseCfg = riderClickhouseCfg
  return HandlerEnv {..}

releaseHandlerEnv :: HandlerEnv -> IO ()
releaseHandlerEnv HandlerEnv {..} = do
  releaseLoggerEnv loggerEnv
  disconnectHedis hedisEnv
  disconnectHedis hedisClusterEnv

type Flow = FlowR HandlerEnv

instance AuthenticatingEntity HandlerEnv where
  getSigningKey = (.signingKey)
  getSignatureExpiry = (.signatureExpiry)
