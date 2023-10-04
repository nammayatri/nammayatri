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
import qualified Data.Map as M
import qualified Data.Text as T
import EulerHS.Prelude
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
import Kernel.Types.Common (HighPrecMeters, Seconds, Tables)
import Kernel.Types.Credentials (PrivateKey)
import Kernel.Types.Flow (FlowR)
import Kernel.Types.Id (Id (..))
import Kernel.Types.Registry
import Kernel.Types.SlidingWindowLimiter
import Kernel.Utils.App (lookupDeploymentVersion)
import Kernel.Utils.Common (CacheConfig)
import Kernel.Utils.Dhall (FromDhall)
import Kernel.Utils.IOLogging
import qualified Kernel.Utils.Registry as Registry
import Kernel.Utils.Servant.Client
import Kernel.Utils.Servant.SignatureAuth
import Lib.Scheduler.Types (SchedulerType)
import Lib.SessionizerMetrics.Prometheus.Internal
import Lib.SessionizerMetrics.Types.Event
import SharedLogic.Allocator (AllocatorJobType)
import SharedLogic.CallBAPInternal
import SharedLogic.External.LocationTrackingService.Types
import SharedLogic.GoogleTranslate
import Storage.CachedQueries.Merchant as CM
import Storage.CachedQueries.RegistryMapFallback as CRM
import System.Environment (lookupEnv)
import Tools.Metrics

-- data Tables = Tables {
--   kVTables :: [Text],
--   kVHardKilledTables :: [Text]
--   }
--   deriving (Generic, Show, ToJSON, FromJSON, FromDhall)
data AppCfg = AppCfg
  { esqDBCfg :: EsqDBConfig,
    esqDBReplicaCfg :: EsqDBConfig,
    hedisMigrationStage :: Bool, -- TODO: remove once data migration is done.
    cutOffHedisCluster :: Bool,
    hedisCfg :: HedisCfg,
    hedisClusterCfg :: HedisCfg,
    hedisNonCriticalCfg :: HedisCfg,
    hedisNonCriticalClusterCfg :: HedisCfg,
    clickhouseCfg :: ClickhouseCfg,
    port :: Int,
    metricsPort :: Int,
    hostName :: Text,
    nwAddress :: BaseUrl,
    selfUIUrl :: BaseUrl,
    signingKey :: PrivateKey,
    signatureExpiry :: Seconds,
    s3Config :: S3Config,
    s3PublicConfig :: S3Config,
    migrationPath :: Maybe FilePath,
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
    driverReachedDistance :: HighPrecMeters,
    cacheTranslationConfig :: CacheTranslationConfig,
    kafkaProducerCfg :: KafkaProducerCfg,
    driverLocationUpdateTopic :: Text,
    broadcastMessageTopic :: Text,
    snapToRoadSnippetThreshold :: HighPrecMeters,
    minTripDistanceForReferralCfg :: Maybe HighPrecMeters,
    maxShards :: Int,
    enableRedisLatencyLogging :: Bool,
    enablePrometheusMetricLogging :: Bool,
    enableAPILatencyLogging :: Bool,
    enableAPIPrometheusMetricLogging :: Bool,
    eventStreamMap :: [EventStreamMap],
    tables :: Tables,
    locationTrackingServiceKey :: Text,
    schedulerSetName :: Text,
    schedulerType :: SchedulerType,
    jobInfoMapx :: M.Map AllocatorJobType Bool,
    ltsCfg :: LocationTrackingeServiceConfig,
    dontEnableForDb :: [Text]
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
    clickhouseEnv :: ClickhouseEnv,
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
    driverReachedDistance :: HighPrecMeters,
    cacheTranslationConfig :: CacheTranslationConfig,
    kafkaProducerCfg :: KafkaProducerCfg,
    kafkaProducerTools :: KafkaProducerTools,
    driverLocationUpdateTopic :: Text,
    broadcastMessageTopic :: Text,
    snapToRoadSnippetThreshold :: HighPrecMeters,
    minTripDistanceForReferralCfg :: Maybe HighPrecMeters,
    maxShards :: Int,
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
    dontEnableForDb :: [Text]
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
  bppMetrics <- registerBPPMetricsContainer metricsSearchDurationTimeout
  ssrMetrics <- registerSendSearchRequestToDriverMetricsContainer
  coreMetrics <- Metrics.registerCoreMetricsContainer
  clickhouseEnv <- createConn clickhouseCfg
  let jobInfoMap :: (M.Map Text Bool) = M.mapKeys show jobInfoMapx
  let searchRequestExpirationSeconds = fromIntegral cfg.searchRequestExpirationSeconds
      driverQuoteExpirationSeconds = fromIntegral cfg.driverQuoteExpirationSeconds
      s3Env = buildS3Env cfg.s3Config
      s3EnvPublic = buildS3Env cfg.s3PublicConfig
  return AppEnv {..}

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
  registryLookup =
    Registry.withSubscriberCache $ \sub ->
      fetchFromDB sub.subscriber_id sub.unique_key_id sub.merchant_id
        >>>= \registryUrl -> Registry.registryLookup registryUrl sub
    where
      fetchFromDB subscriberId uniqueId merchantId = do
        mbRegistryMapFallback <- CRM.findBySubscriberIdAndUniqueId subscriberId uniqueId
        case mbRegistryMapFallback of
          Just registryMapFallback -> pure $ Just registryMapFallback.registryUrl
          Nothing ->
            do
              mbMerchant <- CM.findById (Id merchantId)
              pure ((\merchant -> Just merchant.registryUrl) =<< mbMerchant)

cacheRegistryKey :: Text
cacheRegistryKey = "dynamic-offer-driver-app:registry:"

instance Cache Subscriber Flow where
  type CacheKey Subscriber = SimpleLookupRequest
  getKey = Redis.get . (cacheRegistryKey <>) . lookupRequestToRedisKey
  setKey = Redis.set . (cacheRegistryKey <>) . lookupRequestToRedisKey
  delKey = Redis.del . (cacheRegistryKey <>) . lookupRequestToRedisKey

instance CacheEx Subscriber Flow where
  setKeyEx ttl = (\k v -> Redis.setExp k v ttl.getSeconds) . (cacheRegistryKey <>) . lookupRequestToRedisKey
