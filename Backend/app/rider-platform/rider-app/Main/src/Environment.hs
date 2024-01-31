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
    RideConfig (..),
    buildAppEnv,
    releaseAppEnv,
  )
where

import AWS.S3
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import Domain.Types.FeedbackForm
import EulerHS.Prelude (newEmptyTMVarIO)
import Kernel.External.Encryption (EncTools)
import Kernel.External.Infobip.Types (InfoBIPConfig)
import Kernel.External.Slack.Types (SlackConfig)
import Kernel.Prelude
import Kernel.Sms.Config
import Kernel.Storage.Esqueleto.Config
import Kernel.Storage.Hedis as Redis
import Kernel.Storage.Hedis.AppPrefixes (riderAppPrefix)
import Kernel.Types.App
import Kernel.Types.Cache
import Kernel.Types.Common (HighPrecMeters, Meters, Seconds)
import Kernel.Types.Credentials (PrivateKey)
import Kernel.Types.Error
import Kernel.Types.Flow
import Kernel.Types.Id
import Kernel.Types.Registry
import Kernel.Types.SlidingWindowLimiter
import Kernel.Utils.App (getPodName, lookupDeploymentVersion)
import Kernel.Utils.Common (CacheConfig, fromMaybeM, logDebug)
import Kernel.Utils.Dhall (FromDhall)
import Kernel.Utils.IOLogging
import qualified Kernel.Utils.Registry as Registry
import Kernel.Utils.Servant.Client (HttpClientOptions, RetryCfg)
import Kernel.Utils.Servant.SignatureAuth
import Lib.Scheduler.Types
import Lib.SessionizerMetrics.Prometheus.Internal
import Lib.SessionizerMetrics.Types.Event
import SharedLogic.GoogleTranslate
import SharedLogic.JobScheduler
import qualified Storage.CachedQueries.BlackListOrg as QBlackList
import Storage.CachedQueries.Merchant as CM
import qualified Storage.CachedQueries.WhiteListOrg as QWhiteList
import Tools.Metrics
import Tools.Streaming.Kafka

data AppCfg = AppCfg
  { esqDBCfg :: EsqDBConfig,
    esqDBReplicaCfg :: EsqDBConfig,
    hedisCfg :: HedisCfg,
    hedisClusterCfg :: HedisCfg,
    hedisNonCriticalCfg :: HedisCfg,
    hedisNonCriticalClusterCfg :: HedisCfg,
    cutOffHedisCluster :: Bool,
    cutOffNonCriticalHedisCluster :: Bool,
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
    googleTranslateUrl :: BaseUrl,
    googleTranslateKey :: Text,
    internalAPIKey :: Text,
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
    signatureExpiry :: Seconds,
    disableSignatureAuth :: Bool,
    encTools :: EncTools,
    kafkaProducerCfg :: KafkaProducerCfg,
    nwAddress :: BaseUrl,
    selfUIUrl :: BaseUrl,
    rideCfg :: RideConfig,
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
    maxMessages :: Text,
    incomingAPIResponseTimeout :: Int,
    maxShards :: Int,
    jobInfoMapx :: M.Map RiderJobType Bool,
    schedulerSetName :: Text,
    schedulerType :: SchedulerType,
    internalEndPointMap :: M.Map BaseUrl BaseUrl,
    isBecknSpecVersion2 :: Bool,
    _version :: Text,
    hotSpotExpiry :: Seconds
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
    loggerConfig :: LoggerConfig,
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
    rideCfg :: RideConfig,
    internalAPIKey :: Text,
    dashboardToken :: Text,
    cacheConfig :: CacheConfig,
    cacheTranslationConfig :: CacheTranslationConfig,
    cacheFeedbackFormConfig :: CacheFeedbackFormConfig,
    maxEmergencyNumberCount :: Int,
    minTripDistanceForReferralCfg :: Maybe HighPrecMeters,
    version :: DeploymentVersion,
    enableRedisLatencyLogging :: Bool,
    enablePrometheusMetricLogging :: Bool,
    eventStreamMap :: [EventStreamMap],
    eventRequestCounter :: EventCounterMetric,
    maxMessages :: Text,
    incomingAPIResponseTimeout :: Int,
    maxShards :: Int,
    internalEndPointHashMap :: HM.HashMap BaseUrl BaseUrl,
    isBecknSpecVersion2 :: Bool,
    _version :: Text,
    hotSpotExpiry :: Seconds
  }
  deriving (Generic)

buildAppEnv :: AppCfg -> IO AppEnv
buildAppEnv cfg@AppCfg {..} = do
  hostname <- getPodName
  version <- lookupDeploymentVersion
  isShuttingDown <- newEmptyTMVarIO
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
  hedisNonCriticalEnv <- connectHedis hedisNonCriticalCfg nonCriticalModifierFunc
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
  return AppEnv {..}

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
  registryLookup =
    Registry.withSubscriberCache $ \sub -> do
      fetchFromDB sub.merchant_id >>= \registryUrl -> do
        -- x <- Registry.registryLookup registryUrl sub >>= Registry.whitelisting isWhiteListed
        logDebug $ "Started Registry lookup for " <> " subId xyz "
        subId <- Registry.registryLookup registryUrl sub
        logDebug $ "Registry lookup for " <> " subId xyz " <> show subId
        x <- Registry.whitelisting isWhiteListed subId
        if (isJust x)
          then do
            logDebug $ "Registry lookup for " <> " returned xyz " <> show x
            Registry.registryLookup registryUrl sub >>= Registry.validateWhitelisting validateWhiteListed
          else do
            pure x
    where
      fetchFromDB merchantId = do
        merchant <- CM.findById (Id merchantId) >>= fromMaybeM (MerchantDoesNotExist merchantId)
        pure $ merchant.registryUrl
      isWhiteListed subscriberId = QBlackList.findBySubscriberId (ShortId subscriberId) <&> isNothing
      validateWhiteListed subscriberId = QWhiteList.findBySubscriberId (ShortId subscriberId) <&> isNothing

instance Cache Subscriber Flow where
  type CacheKey Subscriber = SimpleLookupRequest
  getKey = Redis.get . ("taxi-bap:registry:" <>) . lookupRequestToRedisKey
  setKey = Redis.set . ("taxi-bap:registry:" <>) . lookupRequestToRedisKey
  delKey = Redis.del . ("taxi-bap:registry:" <>) . lookupRequestToRedisKey

instance CacheEx Subscriber Flow where
  setKeyEx ttl = (\k v -> Redis.setExp k v ttl.getSeconds) . ("taxi-bap:registry:" <>) . lookupRequestToRedisKey

data RideConfig = RideConfig
  { driverReachedDistance :: Meters,
    driverOnTheWayNotifyExpiry :: Seconds
  }
  deriving (Generic, FromDhall)
