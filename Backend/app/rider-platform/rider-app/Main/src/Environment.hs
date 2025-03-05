{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Environment
  ( Env,
    FlowHandler,
    FlowServer,
    Flow,
    AppCfg (..),
    AppEnv (..),
    BAPs (..),
    buildAppEnv,
    releaseAppEnv,
    cacheRegistryKey,
  )
where

import AWS.S3
import qualified BecknV2.FRFS.Enums as Spec
import qualified BecknV2.OnDemand.Enums as BecknSpec
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Database.PostgreSQL.Simple as PG
import Domain.Types (GatewayAndRegistryService (..))
import Domain.Types.FeedbackForm
import qualified Domain.Types.Merchant as DM
import EulerHS.Prelude (newEmptyTMVarIO, (+||), (||+))
import Kernel.External.Encryption (EncTools)
import Kernel.External.Infobip.Types (InfoBIPConfig)
import Kernel.External.Slack.Types (SlackConfig)
import Kernel.Prelude
import Kernel.Sms.Config
import Kernel.Storage.Clickhouse.Config
import Kernel.Storage.Esqueleto.Config
import Kernel.Storage.Hedis as Redis hiding (ttl)
import Kernel.Storage.Hedis.AppPrefixes (riderAppPrefix)
import Kernel.Types.App
import qualified Kernel.Types.Beckn.Domain as Domain
import Kernel.Types.Cache
import qualified Kernel.Types.CacheFlow as CF
import Kernel.Types.Common (Distance, DistanceUnit (Meter), HighPrecMeters, Seconds, convertHighPrecMetersToDistance)
import Kernel.Types.Credentials (PrivateKey)
import Kernel.Types.Error
import Kernel.Types.Flow
import Kernel.Types.Id
import Kernel.Types.Registry
import Kernel.Types.SlidingWindowLimiter
import Kernel.Utils.App (getPodName, lookupDeploymentVersion)
import Kernel.Utils.Common (CacheConfig, fromMaybeM, logError, throwError)
import Kernel.Utils.Dhall (FromDhall)
import Kernel.Utils.IOLogging
import qualified Kernel.Utils.Registry as Registry
import Kernel.Utils.Servant.Client (HttpClientOptions, RetryCfg)
import Kernel.Utils.Servant.SignatureAuth
import Lib.Scheduler.Types
import Lib.SessionizerMetrics.Prometheus.Internal
import Lib.SessionizerMetrics.Types.Event
import Passetto.Client
import qualified Registry.Beckn.Nammayatri.Types as NyRegistry
import SharedLogic.External.LocationTrackingService.Types
import SharedLogic.GoogleTranslate
import SharedLogic.JobScheduler
import Storage.CachedQueries.Merchant as CM
import qualified Storage.Queries.BecknConfig as QBC
import System.Environment as SE
import Tools.Error
import Tools.Metrics
import Tools.Streaming.Kafka
import TransactionLogs.Types hiding (ONDC)
import qualified UrlShortner.Common as UrlShortner

data AppCfg = AppCfg
  { esqDBCfg :: EsqDBConfig,
    esqDBReplicaCfg :: EsqDBConfig,
    hedisCfg :: HedisCfg,
    hedisClusterCfg :: HedisCfg,
    hedisNonCriticalCfg :: HedisCfg,
    hedisNonCriticalClusterCfg :: HedisCfg,
    cutOffHedisCluster :: Bool,
    cutOffNonCriticalHedisCluster :: Bool,
    riderClickhouseCfg :: ClickhouseCfg,
    kafkaClickhouseCfg :: ClickhouseCfg,
    dashboardClickhouseCfg :: ClickhouseCfg,
    hedisMigrationStage :: Bool,
    smsCfg :: SmsConfig,
    infoBIPCfg :: InfoBIPConfig,
    port :: Int,
    metricsPort :: Int,
    hostName :: Text,
    searchRequestExpiry :: Maybe Seconds,
    migrationPath :: [FilePath],
    autoMigrate :: Bool,
    coreVersion :: Text,
    loggerConfig :: LoggerConfig,
    internalAPIKey :: Text,
    internalClickhouseAPIKey :: Text,
    googleTranslateUrl :: BaseUrl,
    googleTranslateKey :: Text,
    metricsSearchDurationTimeout :: Seconds,
    graceTerminationPeriod :: Seconds,
    apiRateLimitOptions :: APIRateLimitOptions,
    searchRateLimitOptions :: APIRateLimitOptions,
    slackCfg :: SlackConfig,
    searchLimitExceedNotificationTemplate :: Text,
    s3Config :: S3Config,
    s3PublicConfig :: S3Config,
    httpClientOptions :: HttpClientOptions,
    shortDurationRetryCfg :: RetryCfg,
    longDurationRetryCfg :: RetryCfg,
    authTokenCacheExpiry :: Seconds,
    signingKey :: PrivateKey,
    storeRidesTimeLimit :: Int,
    signatureExpiry :: Seconds,
    disableSignatureAuth :: Bool,
    encTools :: EncTools,
    kafkaProducerCfg :: KafkaProducerCfg,
    nwAddress :: BaseUrl,
    selfUIUrl :: BaseUrl,
    dashboardToken :: Text,
    cacheConfig :: CacheConfig,
    cacheTranslationConfig :: CacheTranslationConfig,
    cacheFeedbackFormConfig :: CacheFeedbackFormConfig,
    maxEmergencyNumberCount :: Int,
    minTripDistanceForReferralCfg :: Maybe HighPrecMeters,
    enableRedisLatencyLogging :: Bool,
    enablePrometheusMetricLogging :: Bool,
    eventStreamMap :: [EventStreamMap],
    kvConfigUpdateFrequency :: Int,
    incomingAPIResponseTimeout :: Int,
    maxShards :: Int,
    jobInfoMapx :: M.Map RiderJobType Bool,
    schedulerSetName :: Text,
    schedulerType :: SchedulerType,
    internalEndPointMap :: M.Map BaseUrl BaseUrl,
    _version :: Text,
    hotSpotExpiry :: Seconds,
    collectRouteData :: Bool,
    cacConfig :: CF.CacConfig,
    cacTenants :: [String],
    superPositionConfig :: CF.SuperPositionConfig,
    ondcTokenMap :: M.Map KeyConfig TokenConfig,
    iosValidateEnpoint :: Text,
    isMetroTestTransaction :: Bool,
    urlShortnerConfig :: UrlShortner.UrlShortnerConfig,
    sosAlertsTopicARN :: Text,
    ondcRegistryUrl :: BaseUrl,
    ondcGatewayUrl :: BaseUrl,
    nyRegistryUrl :: BaseUrl,
    nyGatewayUrl :: BaseUrl,
    googleSAPrivateKey :: String,
    ltsCfg :: LocationTrackingeServiceConfig,
    nammayatriRegistryConfig :: NyRegistry.RegistryConfig
  }
  deriving (Generic, FromDhall)

-- TODO coreVersion should be hardcoded in spec, because we can't change coreVersion without changing code
data AppEnv = AppEnv
  { smsCfg :: SmsConfig,
    infoBIPCfg :: InfoBIPConfig,
    jobInfoMap :: M.Map Text Bool,
    schedulerSetName :: Text,
    schedulerType :: SchedulerType,
    hostName :: Text,
    searchRequestExpiry :: Maybe Seconds,
    coreVersion :: Text,
    serviceClickhouseEnv :: ClickhouseEnv,
    kafkaClickhouseEnv :: ClickhouseEnv,
    dashboardClickhouseEnv :: ClickhouseEnv,
    serviceClickhouseCfg :: ClickhouseCfg,
    kafkaClickhouseCfg :: ClickhouseCfg,
    dashboardClickhouseCfg :: ClickhouseCfg,
    loggerConfig :: LoggerConfig,
    internalAPIKey :: Text,
    internalClickhouseAPIKey :: Text,
    googleTranslateUrl :: BaseUrl,
    googleTranslateKey :: Text,
    graceTerminationPeriod :: Seconds,
    apiRateLimitOptions :: APIRateLimitOptions,
    searchRateLimitOptions :: APIRateLimitOptions,
    slackCfg :: SlackConfig,
    searchLimitExceedNotificationTemplate :: Text,
    httpClientOptions :: HttpClientOptions,
    shortDurationRetryCfg :: RetryCfg,
    longDurationRetryCfg :: RetryCfg,
    authTokenCacheExpiry :: Seconds,
    storeRidesTimeLimit :: Int,
    signingKey :: PrivateKey,
    signatureExpiry :: Seconds,
    s3Config :: S3Config,
    s3PublicConfig :: S3Config,
    s3Env :: S3Env Flow,
    s3EnvPublic :: S3Env Flow,
    disableSignatureAuth :: Bool,
    encTools :: EncTools,
    nwAddress :: BaseUrl,
    selfUIUrl :: BaseUrl,
    hedisEnv :: HedisEnv,
    hedisNonCriticalEnv :: HedisEnv,
    hedisNonCriticalClusterEnv :: HedisEnv,
    hedisClusterEnv :: HedisEnv,
    cutOffHedisCluster :: Bool,
    cutOffNonCriticalHedisCluster :: Bool,
    hedisMigrationStage :: Bool,
    esqDBEnv :: EsqDBEnv,
    esqDBReplicaEnv :: EsqDBEnv,
    isShuttingDown :: TMVar (),
    bapMetrics :: BAPMetricsContainer,
    coreMetrics :: CoreMetricsContainer,
    loggerEnv :: LoggerEnv,
    kafkaProducerTools :: KafkaProducerTools,
    kafkaEnvs :: BAPKafkaEnvs,
    dashboardToken :: Text,
    cacheConfig :: CacheConfig,
    cacheTranslationConfig :: CacheTranslationConfig,
    cacheFeedbackFormConfig :: CacheFeedbackFormConfig,
    maxEmergencyNumberCount :: Int,
    minTripDistanceForReferralCfg :: Maybe Distance,
    version :: DeploymentVersion,
    enableRedisLatencyLogging :: Bool,
    enablePrometheusMetricLogging :: Bool,
    eventStreamMap :: [EventStreamMap],
    eventRequestCounter :: EventCounterMetric,
    incomingAPIResponseTimeout :: Int,
    maxShards :: Int,
    internalEndPointHashMap :: HM.HashMap BaseUrl BaseUrl,
    _version :: Text,
    hotSpotExpiry :: Seconds,
    cacConfig :: CF.CacConfig,
    cacTenants :: [String],
    superPositionConfig :: CF.SuperPositionConfig,
    collectRouteData :: Bool,
    shouldLogRequestId :: Bool,
    requestId :: Maybe Text,
    kafkaProducerForART :: Maybe KafkaProducerTools,
    ondcTokenHashMap :: HM.HashMap KeyConfig TokenConfig,
    iosValidateEnpoint :: Text,
    isMetroTestTransaction :: Bool,
    urlShortnerConfig :: UrlShortner.UrlShortnerConfig,
    passettoContext :: PassettoContext,
    sosAlertsTopicARN :: Text,
    psqlConn :: PG.Connection,
    ondcRegistryUrl :: BaseUrl,
    ondcGatewayUrl :: BaseUrl,
    nyRegistryUrl :: BaseUrl,
    nyGatewayUrl :: BaseUrl,
    googleSAPrivateKey :: String,
    ltsCfg :: LocationTrackingeServiceConfig,
    nammayatriRegistryConfig :: NyRegistry.RegistryConfig
  }
  deriving (Generic)

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
buildAppEnv cfg@AppCfg {..} = do
  hostname <- getPodName
  psqlConn <- PG.connect (toConnectInfo esqDBCfg)
  version <- lookupDeploymentVersion
  isShuttingDown <- newEmptyTMVarIO
  passettoContext <- uncurry mkDefPassettoContext encTools.service
  bapMetrics <- registerBAPMetricsContainer metricsSearchDurationTimeout
  coreMetrics <- registerCoreMetricsContainer
  loggerEnv <- prepareLoggerEnv loggerConfig hostname
  esqDBEnv <- prepareEsqDBEnv esqDBCfg loggerEnv
  esqDBReplicaEnv <- prepareEsqDBEnv esqDBReplicaCfg loggerEnv
  eventRequestCounter <- registerEventRequestCounterMetric
  kafkaProducerTools <- buildKafkaProducerTools kafkaProducerCfg
  kafkaEnvs <- buildBAPKafkaEnvs
  let jobInfoMap :: (M.Map Text Bool) = M.mapKeys show jobInfoMapx
  let nonCriticalModifierFunc = ("ab:n_c:" <>)
  hedisEnv <- connectHedis hedisCfg riderAppPrefix
  let requestId = Nothing
  shouldLogRequestId <- fromMaybe False . (>>= readMaybe) <$> SE.lookupEnv "SHOULD_LOG_REQUEST_ID"
  hedisNonCriticalEnv <- connectHedis hedisNonCriticalCfg nonCriticalModifierFunc
  let kafkaProducerForART = Just kafkaProducerTools
  hedisClusterEnv <-
    if cutOffHedisCluster
      then pure hedisEnv
      else connectHedisCluster hedisClusterCfg riderAppPrefix
  hedisNonCriticalClusterEnv <-
    if cutOffNonCriticalHedisCluster
      then pure hedisNonCriticalEnv
      else connectHedisCluster hedisNonCriticalClusterCfg nonCriticalModifierFunc
  let s3Env = buildS3Env cfg.s3Config
      s3EnvPublic = buildS3Env cfg.s3PublicConfig
  let internalEndPointHashMap = HM.fromList $ M.toList internalEndPointMap
  serviceClickhouseEnv <- createConn riderClickhouseCfg
  kafkaClickhouseEnv <- createConn kafkaClickhouseCfg
  dashboardClickhouseEnv <- createConn dashboardClickhouseCfg
  let serviceClickhouseCfg = riderClickhouseCfg
  let ondcTokenHashMap = HM.fromList $ M.toList ondcTokenMap
  return AppEnv {minTripDistanceForReferralCfg = convertHighPrecMetersToDistance Meter <$> minTripDistanceForReferralCfg, ..}

releaseAppEnv :: AppEnv -> IO ()
releaseAppEnv AppEnv {..} = do
  releaseKafkaProducerTools kafkaProducerTools
  releaseLoggerEnv loggerEnv
  disconnectHedis hedisEnv
  disconnectHedis hedisClusterEnv

type Env = EnvR AppEnv

type FlowHandler = FlowHandlerR AppEnv

type FlowServer api = FlowServerR AppEnv api

type Flow = FlowR AppEnv

data BAPs a = BAPs
  { metro :: a,
    cabs :: a
  }
  deriving (Generic, FromDhall)

instance AuthenticatingEntity AppEnv where
  getSigningKey = (.signingKey)
  getSignatureExpiry = (.signatureExpiry)

instance Registry Flow where
  registryLookup = Registry.withSubscriberCache performLookup
    where
      performLookup sub = do
        merchant <- CM.findById (Id sub.merchant_id) >>= fromMaybeM (MerchantDoesNotExist sub.merchant_id)
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
          bapConfig <- QBC.findByMerchantIdDomainAndVehicle (Just merchant.id) (show Spec.FRFS) BecknSpec.METRO >>= fromMaybeM (BecknConfigNotFound $ "MerchantId:" +|| merchant.id.getId ||+ "Domain:" +|| Spec.FRFS ||+ "Vehicle:" +|| BecknSpec.METRO ||+ "")
          let selfSubId = if sub.domain == Domain.PUBLIC_TRANSPORT then bapConfig.subscriberId else merchant.bapId
          Registry.registryLookup registryUrl sub selfSubId
            `catch` \e -> retryWithNextRegistry e registryUrl sub merchant (tryNumber + 1)
      reorderList :: [a] -> [a]
      reorderList [] = []
      reorderList (x : xs) = xs ++ [x]

cacheRegistryKey :: Text
cacheRegistryKey = "taxi-bap:registry:"

instance Cache Subscriber Flow where
  type CacheKey Subscriber = SimpleLookupRequest
  getKey = Redis.get . (cacheRegistryKey <>) . lookupRequestToRedisKey
  setKey = Redis.set . (cacheRegistryKey <>) . lookupRequestToRedisKey
  delKey = Redis.del . (cacheRegistryKey <>) . lookupRequestToRedisKey

instance CacheEx Subscriber Flow where
  setKeyEx ttls = (\k v -> Redis.setExp k v ttls.getSeconds) . (cacheRegistryKey <>) . lookupRequestToRedisKey
