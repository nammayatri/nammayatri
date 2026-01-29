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

import AWS.S3
import qualified Data.HashMap.Strict as HMS
import qualified Data.Map as M
import qualified Data.Map.Strict as MS
import Data.String.Conversions (cs)
import qualified Data.Text as T
import "dynamic-offer-driver-app" Environment (AppCfg (..))
import Kernel.External.Encryption (EncTools)
import Kernel.Prelude
import Kernel.Sms.Config (SmsConfig)
import Kernel.Storage.Clickhouse.Config
import Kernel.Storage.Esqueleto.Config
import Kernel.Storage.Hedis (HedisEnv, connectHedis, connectHedisCluster, disconnectHedis)
import qualified Kernel.Storage.InMem as IM
import Kernel.Streaming.Kafka.Commons
import Kernel.Streaming.Kafka.Producer.Types
import Kernel.Types.Base64 (Base64)
import qualified Kernel.Types.CacheFlow as CF
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
import SharedLogic.CallBAPInternal (AppBackendBapInternal)
import SharedLogic.CallInternalMLPricing (MLPricingInternal)
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import "dynamic-offer-driver-app" SharedLogic.GoogleTranslate
import System.Environment (lookupEnv, setEnv)
import Tools.Metrics
import Tools.Metrics.ARDUBPPMetrics.Types
import TransactionLogs.Types

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
    serviceClickhouseEnv :: ClickhouseEnv,
    loggerEnv :: LoggerEnv,
    esqDBEnv :: EsqDBEnv,
    esqDBReplicaEnv :: EsqDBEnv,
    encTools :: EncTools,
    hedisEnv :: HedisEnv,
    kafkaProducerTools :: KafkaProducerTools,
    hedisNonCriticalEnv :: HedisEnv,
    hedisNonCriticalClusterEnv :: HedisEnv,
    hedisClusterEnv :: HedisEnv,
    secondaryHedisClusterEnv :: Maybe HedisEnv,
    cutOffHedisCluster :: Bool,
    hedisMigrationStage :: Bool,
    cacheConfig :: CacheConfig,
    cacheTranslationConfig :: CacheTranslationConfig,
    googleTranslateUrl :: BaseUrl,
    googleTranslateKey :: Text,
    coreMetrics :: CoreMetricsContainer,
    ssrMetrics :: SendSearchRequestToDriverMetricsContainer,
    maxShards :: Int,
    maxNotificationShards :: Int,
    smsCfg :: SmsConfig,
    version :: DeploymentVersion,
    eventStreamMap :: [EventStreamMap],
    eventRequestCounter :: EventCounterMetric,
    jobInfoMap :: M.Map Text Bool,
    enableRedisLatencyLogging :: Bool,
    enablePrometheusMetricLogging :: Bool,
    ltsCfg :: LT.LocationTrackingeServiceConfig,
    schedulerSetName :: Text,
    kvConfigUpdateFrequency :: Int,
    nwAddress :: BaseUrl,
    internalEndPointHashMap :: HMS.HashMap BaseUrl BaseUrl,
    schedulerType :: SchedulerType,
    requestId :: Maybe Text,
    shouldLogRequestId :: Bool,
    sessionId :: Maybe Text,
    kafkaProducerForART :: Maybe KafkaProducerTools,
    singleBatchProcessingTempDelay :: NominalDiffTime,
    enableAPILatencyLogging :: Bool,
    enableAPIPrometheusMetricLogging :: Bool,
    ondcTokenHashMap :: HMS.HashMap KeyConfig TokenConfig,
    cacConfig :: CacConfig,
    modelNamesHashMap :: HMS.HashMap Text Text,
    searchRequestExpirationSeconds :: NominalDiffTime,
    searchRequestExpirationSecondsForMultimodal :: NominalDiffTime,
    s3Env :: S3Env Flow,
    passettoContext :: PassettoContext,
    serviceClickhouseCfg :: ClickhouseCfg,
    kafkaClickhouseCfg :: ClickhouseCfg,
    broadcastMessageTopic :: KafkaTopic,
    selfBaseUrl :: BaseUrl,
    appBackendBapInternal :: AppBackendBapInternal,
    mlPricingInternal :: MLPricingInternal,
    inMemEnv :: CF.InMemEnv,
    bppMetrics :: BPPMetricsContainer,
    snapToRoadSnippetThreshold :: HighPrecMeters,
    droppedPointsThreshold :: HighPrecMeters,
    osrmMatchThreshold :: HighPrecMeters,
    maxStraightLineRectificationThreshold :: HighPrecMeters,
    loggerConfig :: LoggerConfig,
    url :: Maybe Text,
    blackListedJobs :: [Text],
    dashboardClickhouseCfg :: ClickhouseCfg,
    dashboardClickhouseEnv :: ClickhouseEnv,
    kafkaClickhouseEnv :: ClickhouseEnv,
    ttenTokenCacheExpiry :: Seconds
  }
  deriving (Generic)

buildHandlerEnv :: HandlerCfg -> IO HandlerEnv
buildHandlerEnv HandlerCfg {..} = do
  let AppCfg {..} = appCfg
  setEnv "CITY_SCHEMA" (T.unpack cityDBSchema)
  hostname <- fmap cs <$> lookupEnv "POD_NAME" :: IO (Maybe Text)
  version <- lookupDeploymentVersion
  loggerEnv <- prepareLoggerEnv appCfg.loggerConfig hostname
  esqDBEnv <- prepareEsqDBEnv appCfg.esqDBCfg loggerEnv
  eventRequestCounter <- registerEventRequestCounterMetric
  esqDBReplicaEnv <- prepareEsqDBEnv appCfg.esqDBReplicaCfg loggerEnv
  kafkaProducerTools <- buildKafkaProducerTools appCfg.kafkaProducerCfg
  passettoContext <- uncurry mkDefPassettoContext encTools.service
  hedisEnv <- connectHedis appCfg.hedisCfg ("dynamic-offer-driver-app:" <>)
  hedisNonCriticalEnv <- connectHedis appCfg.hedisNonCriticalCfg ("doa:n_c:" <>)
  serviceClickhouseEnv <- createConn driverClickhouseCfg
  let internalEndPointHashMap = HMS.fromList $ MS.toList internalEndPointMap
  let requestId = Nothing
  shouldLogRequestId <- fromMaybe False . (>>= readMaybe) <$> lookupEnv "SHOULD_LOG_REQUEST_ID"
  let sessionId = Nothing
  let kafkaProducerForART = Just kafkaProducerTools
  hedisClusterEnv <-
    if cutOffHedisCluster
      then pure hedisEnv
      else connectHedisCluster hedisClusterCfg ("dynamic-offer-driver-app:" <>)
  hedisNonCriticalClusterEnv <-
    if cutOffHedisCluster
      then pure hedisNonCriticalEnv
      else connectHedisCluster hedisNonCriticalClusterCfg ("doa:n_c:" <>)
  secondaryHedisClusterEnv <-
    Kernel.Prelude.try (connectHedisCluster appCfg.hedisSecondaryClusterCfg ("dynamic-offer-driver-app:" <>)) >>= \case
      Left (e :: SomeException) -> do
        putStrLn $ "ERROR: Failed to connect to secondary hedis cluster: " ++ show e
        pure Nothing
      Right env -> pure (Just env)
  let jobInfoMap :: (M.Map Text Bool) = M.mapKeys show jobInfoMapx
  ssrMetrics <- registerSendSearchRequestToDriverMetricsContainer
  bppMetrics <- registerBPPMetricsContainer metricsSearchDurationTimeout
  coreMetrics <- registerCoreMetricsContainer
  dashboardClickhouseEnv <- createConn dashboardClickhouseCfg
  kafkaClickhouseEnv <- createConn kafkaClickhouseCfg
  let ondcTokenHashMap = HMS.fromList $ M.toList ondcTokenMap
  let s3Env = buildS3Env s3Config
  let searchRequestExpirationSeconds' = fromIntegral appCfg.searchRequestExpirationSeconds
      serviceClickhouseCfg = driverClickhouseCfg
      searchRequestExpirationSecondsForMultimodal' = fromIntegral appCfg.searchRequestExpirationSecondsForMultimodal
  inMemEnv <- IM.setupInMemEnv inMemConfig (Just hedisClusterEnv)
  let url = Nothing
  return HandlerEnv {modelNamesHashMap = HMS.fromList $ M.toList modelNamesMap, searchRequestExpirationSeconds = searchRequestExpirationSeconds', searchRequestExpirationSecondsForMultimodal = searchRequestExpirationSecondsForMultimodal', ttenTokenCacheExpiry = appCfg.ttenTokenCacheExpiry, ..}

releaseHandlerEnv :: HandlerEnv -> IO ()
releaseHandlerEnv HandlerEnv {..} = do
  releaseLoggerEnv loggerEnv
  disconnectHedis hedisEnv
  disconnectHedis hedisClusterEnv
  maybe (pure ()) disconnectHedis secondaryHedisClusterEnv

type Flow = FlowR HandlerEnv

instance AuthenticatingEntity HandlerEnv where
  getSigningKey = (.signingKey)
  getSignatureExpiry = (.signatureExpiry)
