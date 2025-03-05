{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Environment where

import AWS.S3
import qualified Data.HashMap.Strict as HMS
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Database.PostgreSQL.Simple as PG
import Domain.Types (GatewayAndRegistryService (..))
import qualified Domain.Types.Merchant as DM
import EulerHS.Prelude
import Kernel.External.Encryption (EncTools)
import Kernel.External.Slack.Types (SlackConfig)
import Kernel.Prelude (NominalDiffTime)
import Kernel.Sms.Config
import Kernel.Storage.Clickhouse.Config
import Kernel.Storage.Esqueleto.Config
import Kernel.Storage.Hedis as Redis hiding (ttl)
import Kernel.Streaming.Kafka.Producer.Types
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.App
import Kernel.Types.Cache
import qualified Kernel.Types.CacheFlow as KTC
import Kernel.Types.Common (HighPrecMeters, Seconds)
import Kernel.Types.Credentials (PrivateKey)
import Kernel.Types.Error
import Kernel.Types.Flow (FlowR)
import Kernel.Types.Id
import Kernel.Types.Registry
import Kernel.Types.SlidingWindowLimiter
import Kernel.Utils.App (lookupDeploymentVersion)
import Kernel.Utils.Common (CacheConfig, fromMaybeM, logError, throwError)
import Kernel.Utils.Dhall (FromDhall)
import Kernel.Utils.IOLogging
import qualified Kernel.Utils.Registry as Registry
import Kernel.Utils.Servant.Client
import Kernel.Utils.Servant.SignatureAuth
import Lib.Scheduler.Types (SchedulerType)
import Lib.SessionizerMetrics.Prometheus.Internal
import Lib.SessionizerMetrics.Types.Event
import Passetto.Client
import qualified Registry.Beckn.Nammayatri.Types as NyRegistry
import SharedLogic.Allocator (AllocatorJobType)
import SharedLogic.CallBAPInternal
import SharedLogic.External.LocationTrackingService.Types
import SharedLogic.GoogleTranslate
import Storage.CachedQueries.Merchant as CM
import Storage.CachedQueries.RegistryMapFallback as CRM
import System.Environment (lookupEnv)
import Tools.Metrics
import TransactionLogs.Types hiding (ONDC)
import qualified UrlShortner.Common as UrlShortner

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
    dashboardClickhouseCfg :: ClickhouseCfg,
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
    superPositionConfig :: KTC.SuperPositionConfig,
    maxStraightLineRectificationThreshold :: HighPrecMeters,
    singleBatchProcessingTempDelay :: NominalDiffTime,
    ondcTokenMap :: M.Map KeyConfig TokenConfig,
    iosValidateEnpoint :: Text,
    quoteRespondCoolDown :: Int,
    sosAlertsTopicARN :: Text,
    ondcRegistryUrl :: BaseUrl,
    ondcGatewayUrl :: BaseUrl,
    nyRegistryUrl :: BaseUrl,
    nyGatewayUrl :: BaseUrl,
    nammayatriRegistryConfig :: NyRegistry.RegistryConfig,
    urlShortnerConfig :: UrlShortner.UrlShortnerConfig
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
    dashboardClickhouseEnv :: ClickhouseEnv,
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
    superPositionConfig :: KTC.SuperPositionConfig,
    requestId :: Maybe Text,
    shouldLogRequestId :: Bool,
    kafkaProducerForART :: Maybe KafkaProducerTools,
    maxStraightLineRectificationThreshold :: HighPrecMeters,
    singleBatchProcessingTempDelay :: NominalDiffTime,
    ondcTokenHashMap :: HMS.HashMap KeyConfig TokenConfig,
    iosValidateEnpoint :: Text,
    passettoContext :: PassettoContext,
    quoteRespondCoolDown :: Int,
    sosAlertsTopicARN :: Text,
    psqlConn :: PG.Connection,
    serviceClickhouseCfg :: ClickhouseCfg,
    dashboardClickhouseCfg :: ClickhouseCfg,
    kafkaClickhouseCfg :: ClickhouseCfg,
    ondcRegistryUrl :: BaseUrl,
    ondcGatewayUrl :: BaseUrl,
    nyRegistryUrl :: BaseUrl,
    nyGatewayUrl :: BaseUrl,
    nammayatriRegistryConfig :: NyRegistry.RegistryConfig,
    urlShortnerConfig :: UrlShortner.UrlShortnerConfig
  }
  deriving (Generic)

instance AuthenticatingEntity AppEnv where
  getSigningKey = (.signingKey)
  getSignatureExpiry = (.signatureExpiry)

toConnectInfo :: EsqDBConfig -> ConnectInfo
toConnectInfo config =
  ConnectInfo
    { connectHost = T.unpack config.connectHost,
      connectPort = config.connectPort,
      connectUser = T.unpack config.connectUser,
      connectPassword = T.unpack config.connectPassword,
      connectDatabase = T.unpack config.connectDatabase
    }

buildAppEnv :: AppCfg -> IO AppEnv
buildAppEnv cfg@AppCfg {searchRequestExpirationSeconds = _searchRequestExpirationSeconds, driverQuoteExpirationSeconds = _driverQuoteExpirationSeconds, ..} = do
  hostname <- map T.pack <$> lookupEnv "POD_NAME"
  psqlConn <- PG.connect (toConnectInfo esqDBCfg)
  version <- lookupDeploymentVersion
  passettoContext <- (uncurry mkDefPassettoContext) encTools.service
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
  let requestId = Nothing
  shouldLogRequestId <- fromMaybe False . (>>= readMaybe) <$> lookupEnv "SHOULD_LOG_REQUEST_ID"
  let kafkaProducerForART = Just kafkaProducerTools
  bppMetrics <- registerBPPMetricsContainer metricsSearchDurationTimeout
  ssrMetrics <- registerSendSearchRequestToDriverMetricsContainer
  coreMetrics <- Metrics.registerCoreMetricsContainer
  kafkaClickhouseEnv <- createConn kafkaClickhouseCfg
  serviceClickhouseEnv <- createConn driverClickhouseCfg
  dashboardClickhouseEnv <- createConn dashboardClickhouseCfg
  let jobInfoMap :: (M.Map Text Bool) = M.mapKeys show jobInfoMapx
  let searchRequestExpirationSeconds = fromIntegral cfg.searchRequestExpirationSeconds
      driverQuoteExpirationSeconds = fromIntegral cfg.driverQuoteExpirationSeconds
      s3Env = buildS3Env cfg.s3Config
      s3EnvPublic = buildS3Env cfg.s3PublicConfig
  let internalEndPointHashMap = HMS.fromList $ M.toList internalEndPointMap
  let ondcTokenHashMap = HMS.fromList $ M.toList ondcTokenMap
      serviceClickhouseCfg = driverClickhouseCfg
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
  registryLookup = Registry.withSubscriberCache performLookup
    where
      performLookup sub = do
        mbRegistryMapFallback <- CRM.findBySubscriberIdAndUniqueId sub.subscriber_id sub.unique_key_id
        merchant <- CM.findById (Id sub.merchant_id) >>= fromMaybeM (MerchantDoesNotExist sub.merchant_id)
        case mbRegistryMapFallback of
          Just registryMapFallback -> do
            Registry.registryLookup registryMapFallback.registryUrl sub merchant.subscriberId.getShortId
          Nothing -> do
            performRegistryLookup merchant.gatewayAndRegistryPriorityList sub merchant 1
      fetchUrlFromList :: [Domain.Types.GatewayAndRegistryService] -> Flow BaseUrl
      fetchUrlFromList priorityList = do
        case priorityList of
          (NY : _) -> asks (.nyRegistryUrl)
          _ -> asks (.ondcRegistryUrl)
      retryWithNextRegistry :: ExternalAPICallError -> BaseUrl -> SimpleLookupRequest -> DM.Merchant -> Int -> Flow (Maybe Subscriber)
      retryWithNextRegistry _ registryUrl sub merchant tryNumber = do
        logError $ "registry " <> show registryUrl <> " seems down, trying with next registryUrl"
        let maxRetries = length merchant.gatewayAndRegistryPriorityList
        if tryNumber > maxRetries
          then throwError $ InternalError "Max retries reached, perhaps all registries are down"
          else do
            let networkPriorityList = reorderList merchant.gatewayAndRegistryPriorityList
            performRegistryLookup networkPriorityList sub merchant tryNumber
      performRegistryLookup :: [Domain.Types.GatewayAndRegistryService] -> SimpleLookupRequest -> DM.Merchant -> Int -> Flow (Maybe Subscriber)
      performRegistryLookup priorityList sub merchant tryNumber = do
        fetchUrlFromList priorityList >>= \registryUrl -> do
          Registry.registryLookup registryUrl sub merchant.subscriberId.getShortId
            `catch` \e -> retryWithNextRegistry e registryUrl sub merchant (tryNumber + 1)
      reorderList :: [a] -> [a]
      reorderList [] = []
      reorderList (x : xs) = xs ++ [x]

cacheRegistryKey :: Text
cacheRegistryKey = "dynamic-offer-driver-app:registry:"

instance Cache Subscriber Flow where
  type CacheKey Subscriber = SimpleLookupRequest
  getKey = Redis.get . (cacheRegistryKey <>) . lookupRequestToRedisKey
  setKey = Redis.set . (cacheRegistryKey <>) . lookupRequestToRedisKey
  delKey = Redis.del . (cacheRegistryKey <>) . lookupRequestToRedisKey

instance CacheEx Subscriber Flow where
  setKeyEx ttl = (\k v -> Redis.setExp k v ttl.getSeconds) . (cacheRegistryKey <>) . lookupRequestToRedisKey
