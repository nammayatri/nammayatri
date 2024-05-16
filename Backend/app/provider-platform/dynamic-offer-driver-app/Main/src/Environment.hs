{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Environment where

import AWS.S3
import qualified Data.HashMap.Strict as HMS
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import EulerHS.Prelude
import Kernel.Beam.Functions (getArtDbFunctions, getDbFunctions)
import Kernel.External.Encryption (EncTools)
import Kernel.External.Slack.Types (SlackConfig)
import Kernel.Prelude (NominalDiffTime, (>>>=))
import Kernel.Sms.Config
import Kernel.Storage.Clickhouse.Config
import Kernel.Storage.Esqueleto.Config
import Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.App
import Kernel.Types.Cache
import qualified Kernel.Types.CacheFlow as KTC
import Kernel.Types.Credentials (PrivateKey)
import Kernel.Types.Flow (FlowR)
import Kernel.Types.Id
import Kernel.Types.Registry
import Kernel.Types.SlidingWindowLimiter
import Kernel.Utils.App (lookupDeploymentVersion)
import Kernel.Utils.Common
import Kernel.Utils.Dhall (FromDhall)
import Kernel.Utils.IOLogging
import qualified Kernel.Utils.Registry as Registry
import Kernel.Utils.Servant.SignatureAuth
import Lib.Scheduler.Types (SchedulerType)
import Lib.SessionizerMetrics.Prometheus.Internal
import Lib.SessionizerMetrics.Types.Event
import SharedLogic.Allocator (AllocatorJobType)
import SharedLogic.CallBAPInternal
import SharedLogic.External.LocationTrackingService.Types
import SharedLogic.GoogleTranslate
import qualified Storage.CachedQueries.BlackListOrg as QBlackList
import Storage.CachedQueries.Merchant as CM
import Storage.CachedQueries.RegistryMapFallback as CRM
import qualified Storage.CachedQueries.WhiteListOrg as QWhiteList
import System.Environment (lookupEnv)
import Tools.Metrics
import TransactionLogs.Types

data SuperPositionConfig = SuperPositionConfig
  { host :: String,
    interval :: Natural,
    tenants :: [String],
    retryConnection :: Bool
  }
  deriving (Generic, FromDhall)

data AppCfg = AppCfg
  { esqDBCfg :: EsqDBConfig,
    esqDBReplicaCfg :: EsqDBConfig,
    hedisMigrationStage :: Bool, -- TODO: remove once data migration is done.
    cutOffHedisCluster :: Bool,
    hedisCfg :: HedisCfg,
    hedisClusterCfg :: HedisCfg,
    hedisNonCriticalCfg :: HedisCfg,
    hedisNonCriticalClusterCfg :: HedisCfg,
    kafkaClickhouseCfg :: ClickhouseCfg,
    driverClickhouseCfg :: ClickhouseCfg,
    port :: Int,
    metricsPort :: Int,
    hostName :: Text,
    nwAddress :: BaseUrl,
    selfUIUrl :: BaseUrl,
    signingKey :: PrivateKey,
    signatureExpiry :: Seconds,
    s3Config :: S3Config,
    s3PublicConfig :: S3Config,
    migrationPath :: [FilePath],
    autoMigrate :: Bool,
    coreVersion :: Text,
    loggerConfig :: LoggerConfig,
    graceTerminationPeriod :: Seconds,
    encTools :: EncTools,
    authTokenCacheExpiry :: Seconds,
    disableSignatureAuth :: Bool,
    smsCfg :: SmsConfig,
    slackCfg :: SlackConfig,
    apiRateLimitOptions :: APIRateLimitOptions,
    googleTranslateUrl :: BaseUrl,
    googleTranslateKey :: Text,
    appBackendBapInternal :: AppBackendBapInternal,
    searchRequestExpirationSeconds :: Int,
    driverQuoteExpirationSeconds :: Int,
    httpClientOptions :: HttpClientOptions,
    shortDurationRetryCfg :: RetryCfg,
    longDurationRetryCfg :: RetryCfg,
    driverUnlockDelay :: Seconds,
    dashboardToken :: Text,
    cacheConfig :: CacheConfig,
    metricsSearchDurationTimeout :: Seconds,
    driverLocationUpdateRateLimitOptions :: APIRateLimitOptions,
    cacheTranslationConfig :: CacheTranslationConfig,
    kafkaProducerCfg :: KafkaProducerCfg,
    driverLocationUpdateTopic :: Text,
    broadcastMessageTopic :: Text,
    snapToRoadSnippetThreshold :: HighPrecMeters,
    droppedPointsThreshold :: HighPrecMeters,
    osrmMatchThreshold :: HighPrecMeters,
    minTripDistanceForReferralCfg :: Maybe HighPrecMeters,
    maxShards :: Int,
    maxNotificationShards :: Int,
    enableRedisLatencyLogging :: Bool,
    enablePrometheusMetricLogging :: Bool,
    enableAPILatencyLogging :: Bool,
    enableAPIPrometheusMetricLogging :: Bool,
    eventStreamMap :: [EventStreamMap],
    kvConfigUpdateFrequency :: Int,
    locationTrackingServiceKey :: Text,
    schedulerSetName :: Text,
    schedulerType :: SchedulerType,
    jobInfoMapx :: M.Map AllocatorJobType Bool,
    ltsCfg :: LocationTrackingeServiceConfig,
    modelNamesMap :: M.Map Text Text,
    incomingAPIResponseTimeout :: Int,
    internalEndPointMap :: M.Map BaseUrl BaseUrl,
    _version :: Text,
    cacConfig :: KTC.CacConfig,
    cacTenants :: [String],
    superPositionConfig :: SuperPositionConfig,
    maxStraightLineRectificationThreshold :: HighPrecMeters,
    singleBatchProcessingTempDelay :: NominalDiffTime,
    ondcTokenMap :: M.Map KeyConfig TokenConfig,
    iosValidateEnpoint :: Text
  }
  deriving (Generic, FromDhall)

data AppEnv = AppEnv
  { hostName :: Text,
    jobInfoMap :: M.Map Text Bool,
    nwAddress :: BaseUrl,
    signingKey :: PrivateKey,
    selfUIUrl :: BaseUrl,
    signatureExpiry :: Seconds,
    coreVersion :: Text,
    loggerConfig :: LoggerConfig,
    s3Config :: S3Config,
    s3PublicConfig :: S3Config,
    graceTerminationPeriod :: Seconds,
    disableSignatureAuth :: Bool,
    esqDBEnv :: EsqDBEnv,
    esqDBReplicaEnv :: EsqDBEnv,
    kafkaClickhouseEnv :: ClickhouseEnv,
    serviceClickhouseEnv :: ClickhouseEnv,
    hedisMigrationStage :: Bool,
    cutOffHedisCluster :: Bool,
    hedisEnv :: HedisEnv,
    hedisNonCriticalEnv :: HedisEnv,
    hedisNonCriticalClusterEnv :: HedisEnv,
    hedisClusterEnv :: HedisEnv,
    isShuttingDown :: TMVar (),
    loggerEnv :: LoggerEnv,
    encTools :: EncTools,
    authTokenCacheExpiry :: Seconds,
    port :: Int,
    coreMetrics :: Metrics.CoreMetricsContainer,
    httpClientOptions :: HttpClientOptions,
    shortDurationRetryCfg :: RetryCfg,
    longDurationRetryCfg :: RetryCfg,
    smsCfg :: SmsConfig,
    slackCfg :: SlackConfig,
    apiRateLimitOptions :: APIRateLimitOptions,
    googleTranslateUrl :: BaseUrl,
    appBackendBapInternal :: AppBackendBapInternal,
    googleTranslateKey :: Text,
    bppMetrics :: BPPMetricsContainer,
    ssrMetrics :: SendSearchRequestToDriverMetricsContainer,
    searchRequestExpirationSeconds :: NominalDiffTime,
    driverQuoteExpirationSeconds :: NominalDiffTime,
    driverUnlockDelay :: Seconds,
    dashboardToken :: Text,
    cacheConfig :: CacheConfig,
    s3Env :: S3Env Flow,
    s3EnvPublic :: S3Env Flow,
    driverLocationUpdateRateLimitOptions :: APIRateLimitOptions,
    cacheTranslationConfig :: CacheTranslationConfig,
    kafkaProducerCfg :: KafkaProducerCfg,
    kafkaProducerTools :: KafkaProducerTools,
    driverLocationUpdateTopic :: Text,
    broadcastMessageTopic :: Text,
    snapToRoadSnippetThreshold :: HighPrecMeters,
    droppedPointsThreshold :: HighPrecMeters,
    osrmMatchThreshold :: HighPrecMeters,
    minTripDistanceForReferralCfg :: Maybe HighPrecMeters,
    maxShards :: Int,
    maxNotificationShards :: Int,
    version :: Metrics.DeploymentVersion,
    enableRedisLatencyLogging :: Bool,
    enablePrometheusMetricLogging :: Bool,
    enableAPILatencyLogging :: Bool,
    enableAPIPrometheusMetricLogging :: Bool,
    eventStreamMap :: [EventStreamMap],
    locationTrackingServiceKey :: Text,
    eventRequestCounter :: EventCounterMetric,
    schedulerSetName :: Text,
    schedulerType :: SchedulerType,
    ltsCfg :: LocationTrackingeServiceConfig,
    modelNamesHashMap :: HMS.HashMap Text Text,
    incomingAPIResponseTimeout :: Int,
    internalEndPointHashMap :: HMS.HashMap BaseUrl BaseUrl,
    _version :: Text,
    cacConfig :: KTC.CacConfig,
    cacTenants :: [String],
    superPositionConfig :: SuperPositionConfig,
    requestId :: Maybe Text,
    shouldLogRequestId :: Bool,
    kafkaProducerForART :: Maybe KafkaProducerTools,
    isArtReplayerEnabled :: Bool,
    dbFunctions :: DbFunctions,
    maxStraightLineRectificationThreshold :: HighPrecMeters,
    singleBatchProcessingTempDelay :: NominalDiffTime,
    ondcTokenHashMap :: HMS.HashMap KeyConfig TokenConfig,
    iosValidateEnpoint :: Text
  }
  deriving (Generic)

instance AuthenticatingEntity AppEnv where
  getSigningKey = (.signingKey)
  getSignatureExpiry = (.signatureExpiry)

buildAppEnv :: AppCfg -> IO AppEnv
buildAppEnv cfg@AppCfg {..} = do
  hostname <- map T.pack <$> lookupEnv "POD_NAME"
  version <- lookupDeploymentVersion
  isShuttingDown <- newEmptyTMVarIO
  loggerEnv <- prepareLoggerEnv loggerConfig hostname
  esqDBEnv <- prepareEsqDBEnv esqDBCfg loggerEnv
  kafkaProducerTools <- buildKafkaProducerTools kafkaProducerCfg
  esqDBReplicaEnv <- prepareEsqDBEnv esqDBReplicaCfg loggerEnv
  isArtReplayerEnabled <- fromMaybe False . (>>= readMaybe) <$> lookupEnv "IS_ART_REPLAYER_ENABLED"
  let dbFunctions = if isArtReplayerEnabled then getArtDbFunctions else getDbFunctions
  eventRequestCounter <- registerEventRequestCounterMetric
  let modifierFunc = ("dynamic-offer-driver-app:" <>)
  hedisEnv <- connectHedis hedisCfg modifierFunc -- will be depreciated once data is migrated to cluster
  hedisNonCriticalEnv <- connectHedis hedisNonCriticalCfg modifierFunc
  hedisClusterEnv <-
    if cutOffHedisCluster
      then pure hedisEnv
      else connectHedisCluster hedisClusterCfg modifierFunc
  hedisNonCriticalClusterEnv <-
    if cutOffHedisCluster
      then pure hedisNonCriticalEnv
      else connectHedisCluster hedisNonCriticalClusterCfg modifierFunc
  let requestId = Nothing
  shouldLogRequestId <- fromMaybe False . (>>= readMaybe) <$> lookupEnv "SHOULD_LOG_REQUEST_ID"
  let kafkaProducerForART = Just kafkaProducerTools
  bppMetrics <- registerBPPMetricsContainer metricsSearchDurationTimeout
  ssrMetrics <- registerSendSearchRequestToDriverMetricsContainer
  coreMetrics <- Metrics.registerCoreMetricsContainer
  kafkaClickhouseEnv <- createConn kafkaClickhouseCfg
  serviceClickhouseEnv <- createConn driverClickhouseCfg
  let jobInfoMap :: (M.Map Text Bool) = M.mapKeys show jobInfoMapx
  let searchRequestExpirationSeconds = fromIntegral cfg.searchRequestExpirationSeconds
      driverQuoteExpirationSeconds = fromIntegral cfg.driverQuoteExpirationSeconds
      s3Env = buildS3Env cfg.s3Config
      s3EnvPublic = buildS3Env cfg.s3PublicConfig
  let internalEndPointHashMap = HMS.fromList $ M.toList internalEndPointMap
  -- let tokenMap :: (M.Map KeyConfig (Text, BaseUrl)) = M.map (\TokenConfig {..} -> (token, ondcUrl)) ondcTokenMap
  let ondcTokenHashMap = HMS.fromList $ M.toList ondcTokenMap
  return AppEnv {modelNamesHashMap = HMS.fromList $ M.toList modelNamesMap, ..}

releaseAppEnv :: AppEnv -> IO ()
releaseAppEnv AppEnv {..} = do
  -- FIXME: disconnect database?
  releaseLoggerEnv loggerEnv
  disconnectHedis hedisEnv
  disconnectHedis hedisClusterEnv

type Env = EnvR AppEnv

type FlowHandler = FlowHandlerR AppEnv

type FlowServer api = FlowServerR AppEnv api

type Flow = FlowR AppEnv

instance Registry Flow where
  registryLookup req = do
    mbSubscriber <- Registry.withSubscriberCache performLookup req

    totalSubIds <- QWhiteList.countTotalSubscribers
    if totalSubIds == 0
      then do
        Registry.checkBlacklisted isBlackListed mbSubscriber
      else do
        Registry.checkWhitelisted isNotWhiteListed req.merchant_id mbSubscriber
    where
      performLookup sub =
        fetchFromDB sub.subscriber_id sub.unique_key_id sub.merchant_id >>>= \registryUrl ->
          Registry.registryLookup registryUrl sub
      fetchFromDB subscriberId uniqueId merchantId = do
        mbRegistryMapFallback <- CRM.findBySubscriberIdAndUniqueId subscriberId uniqueId
        case mbRegistryMapFallback of
          Just registryMapFallback -> pure $ Just registryMapFallback.registryUrl
          Nothing ->
            do
              mbMerchant <- CM.findById (Id merchantId)
              pure ((\merchant -> Just merchant.registryUrl) =<< mbMerchant)
      isBlackListed subscriberId domain = QBlackList.findBySubscriberIdAndDomain (ShortId subscriberId) domain <&> isJust
      isNotWhiteListed subscriberId domain _merchantId = QWhiteList.findBySubscriberIdAndDomain (ShortId subscriberId) domain <&> isNothing

cacheRegistryKey :: Text
cacheRegistryKey = "dynamic-offer-driver-app:registry:"

instance Cache Subscriber Flow where
  type CacheKey Subscriber = SimpleLookupRequest
  getKey = Redis.get . (cacheRegistryKey <>) . lookupRequestToRedisKey
  setKey = Redis.set . (cacheRegistryKey <>) . lookupRequestToRedisKey
  delKey = Redis.del . (cacheRegistryKey <>) . lookupRequestToRedisKey

instance CacheEx Subscriber Flow where
  setKeyEx ttl = (\k v -> Redis.setExp k v ttl.getSeconds) . (cacheRegistryKey <>) . lookupRequestToRedisKey
