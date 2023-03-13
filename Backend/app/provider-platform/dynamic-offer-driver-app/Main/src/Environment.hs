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
import qualified Data.Text as T
import qualified Domain.Action.UI.Ride.EndRide.DefaultConfig as EndRideDefCfg
import EulerHS.Prelude
import Kernel.External.Encryption (EncTools)
import Kernel.External.Slack.Types (SlackConfig)
import Kernel.Prelude (NominalDiffTime)
import Kernel.Sms.Config
import Kernel.Storage.Clickhouse.Config
import Kernel.Storage.Esqueleto.Config
import Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.App
import Kernel.Types.Cache
import Kernel.Types.Common
import Kernel.Types.Credentials (PrivateKey)
import Kernel.Types.Flow (FlowR)
import Kernel.Types.Registry
import Kernel.Types.SlidingWindowLimiter
import Kernel.Utils.Dhall (FromDhall)
import Kernel.Utils.IOLogging
import qualified Kernel.Utils.Registry as Registry
import Kernel.Utils.Servant.Client
import Kernel.Utils.Servant.SignatureAuth
import SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Config (SendSearchRequestJobConfig)
import SharedLogic.DriverPool (CancellationScoreRelatedConfig, DriverPoolConfig, IntelligentPoolConfig, OverrideDriverPoolConfig)
import SharedLogic.GoogleTranslate
import Storage.CachedQueries.CacheConfig
import System.Environment (lookupEnv)
import Tools.Metrics

data AppCfg = AppCfg
  { esqDBCfg :: EsqDBConfig,
    esqDBReplicaCfg :: EsqDBConfig,
    hedisCfg :: HedisCfg,
    clickhouseCfg :: ClickhouseCfg,
    port :: Int,
    metricsPort :: Int,
    hostName :: Text,
    nwAddress :: BaseUrl,
    selfUIUrl :: BaseUrl,
    signingKey :: PrivateKey,
    signatureExpiry :: Seconds,
    s3Config :: S3Config,
    migrationPath :: Maybe FilePath,
    autoMigrate :: Bool,
    coreVersion :: Text,
    loggerConfig :: LoggerConfig,
    graceTerminationPeriod :: Seconds,
    registryUrl :: BaseUrl,
    encTools :: EncTools,
    authTokenCacheExpiry :: Seconds,
    minimumDriverRatesCount :: Int,
    disableSignatureAuth :: Bool,
    otpSmsTemplate :: Text,
    smsCfg :: SmsConfig,
    driverOnboardingConfigs :: DriverOnboardingConfigs,
    slackCfg :: SlackConfig,
    apiRateLimitOptions :: APIRateLimitOptions,
    googleTranslateUrl :: BaseUrl,
    googleTranslateKey :: Text,
    searchRequestExpirationSeconds :: Int,
    driverQuoteExpirationSeconds :: Int,
    httpClientOptions :: HttpClientOptions,
    shortDurationRetryCfg :: RetryCfg,
    longDurationRetryCfg :: RetryCfg,
    driverUnlockDelay :: Seconds,
    dashboardToken :: Text,
    defaultEndRideCfg :: EndRideDefCfg.EndRideDefaultConfig,
    cacheConfig :: CacheConfig,
    metricsSearchDurationTimeout :: Seconds,
    driverLocationUpdateRateLimitOptions :: APIRateLimitOptions,
    driverReachedDistance :: HighPrecMeters,
    cacheTranslationConfig :: CacheTranslationConfig,
    driverPoolCfg :: DriverPoolConfig,
    intelligentPoolConfig :: IntelligentPoolConfig,
    defaultPopupDelay :: Seconds,
    cancellationScoreRelatedConfig :: CancellationScoreRelatedConfig,
    overrideDriverPoolCfg :: Maybe [OverrideDriverPoolConfig],
    sendSearchRequestJobCfg :: SendSearchRequestJobConfig,
    kafkaProducerCfg :: KafkaProducerCfg,
    driverLocationUpdateTopic :: Text,
    broadcastMessageTopic :: Text,
    maxParallelSearchRequests :: Int,
    snapToRoadSnippetThreshold :: HighPrecMeters,
    mediaFileUrlPattern :: Text,
    minTripDistanceForReferralCfg :: Maybe HighPrecMeters
  }
  deriving (Generic, FromDhall)

data AppEnv = AppEnv
  { hostName :: Text,
    nwAddress :: BaseUrl,
    signingKey :: PrivateKey,
    selfUIUrl :: BaseUrl,
    signatureExpiry :: Seconds,
    coreVersion :: Text,
    loggerConfig :: LoggerConfig,
    s3Config :: S3Config,
    graceTerminationPeriod :: Seconds,
    registryUrl :: BaseUrl,
    disableSignatureAuth :: Bool,
    esqDBEnv :: EsqDBEnv,
    esqDBReplicaEnv :: EsqDBEnv,
    clickhouseEnv :: ClickhouseEnv,
    hedisEnv :: HedisEnv,
    isShuttingDown :: TMVar (),
    loggerEnv :: LoggerEnv,
    encTools :: EncTools,
    authTokenCacheExpiry :: Seconds,
    minimumDriverRatesCount :: Int,
    port :: Int,
    coreMetrics :: Metrics.CoreMetricsContainer,
    httpClientOptions :: HttpClientOptions,
    shortDurationRetryCfg :: RetryCfg,
    longDurationRetryCfg :: RetryCfg,
    otpSmsTemplate :: Text,
    smsCfg :: SmsConfig,
    driverOnboardingConfigs :: DriverOnboardingConfigs,
    slackCfg :: SlackConfig,
    apiRateLimitOptions :: APIRateLimitOptions,
    googleTranslateUrl :: BaseUrl,
    googleTranslateKey :: Text,
    bppMetrics :: BPPMetricsContainer,
    ssrMetrics :: SendSearchRequestToDriverMetricsContainer,
    searchRequestExpirationSeconds :: NominalDiffTime,
    driverQuoteExpirationSeconds :: NominalDiffTime,
    driverUnlockDelay :: Seconds,
    dashboardToken :: Text,
    defaultEndRideCfg :: EndRideDefCfg.EndRideDefaultConfig,
    cacheConfig :: CacheConfig,
    s3Env :: S3Env Flow,
    driverLocationUpdateRateLimitOptions :: APIRateLimitOptions,
    driverReachedDistance :: HighPrecMeters,
    cacheTranslationConfig :: CacheTranslationConfig,
    driverPoolCfg :: DriverPoolConfig,
    intelligentPoolConfig :: IntelligentPoolConfig,
    defaultPopupDelay :: Seconds,
    cancellationScoreRelatedConfig :: CancellationScoreRelatedConfig,
    overrideDriverPoolConfig :: [OverrideDriverPoolConfig],
    sendSearchRequestJobCfg :: SendSearchRequestJobConfig,
    kafkaProducerCfg :: KafkaProducerCfg,
    kafkaProducerTools :: KafkaProducerTools,
    driverLocationUpdateTopic :: Text,
    broadcastMessageTopic :: Text,
    maxParallelSearchRequests :: Int,
    snapToRoadSnippetThreshold :: HighPrecMeters,
    mediaFileUrlPattern :: Text,
    minTripDistanceForReferralCfg :: Maybe HighPrecMeters
  }
  deriving (Generic)

data DriverOnboardingConfigs = DriverOnboardingConfigs
  { onboardingTryLimit :: Int,
    onboardingRetryTimeinHours :: Int,
    onboardSupportSmsTemplate :: Text,
    checkRCInsuranceExpiry :: Bool,
    checkRCExpiry :: Bool,
    checkRCVehicleClass :: Bool,
    checkDLExpiry :: Bool,
    checkDLVehicleClass :: Bool,
    checkImageExtraction :: Bool,
    checkImageExtractionForDashboard :: Bool,
    validDLVehicleClassInfixes :: [Text]
  }
  deriving (Generic, FromDhall)

instance AuthenticatingEntity AppEnv where
  getSigningKey = (.signingKey)
  getSignatureExpiry = (.signatureExpiry)

buildAppEnv :: AppCfg -> IO AppEnv
buildAppEnv cfg@AppCfg {..} = do
  hostname <- map T.pack <$> lookupEnv "POD_NAME"
  isShuttingDown <- newEmptyTMVarIO
  loggerEnv <- prepareLoggerEnv loggerConfig hostname
  esqDBEnv <- prepareEsqDBEnv esqDBCfg loggerEnv
  kafkaProducerTools <- buildKafkaProducerTools kafkaProducerCfg
  esqDBReplicaEnv <- prepareEsqDBEnv esqDBReplicaCfg loggerEnv
  let modifierFunc = ("dynamic-offer-driver-app:" <>)
  hedisEnv <- connectHedis hedisCfg modifierFunc
  bppMetrics <- registerBPPMetricsContainer metricsSearchDurationTimeout
  ssrMetrics <- registerSendSearchRequestToDriverMetricsContainer
  coreMetrics <- Metrics.registerCoreMetricsContainer
  clickhouseEnv <- createConn clickhouseCfg
  let searchRequestExpirationSeconds = fromIntegral cfg.searchRequestExpirationSeconds
      driverQuoteExpirationSeconds = fromIntegral cfg.driverQuoteExpirationSeconds
      s3Env = buildS3Env cfg.s3Config
      overrideDriverPoolConfig = fromMaybe [] overrideDriverPoolCfg
  return AppEnv {..}

releaseAppEnv :: AppEnv -> IO ()
releaseAppEnv AppEnv {..} = do
  -- FIXME: disconnect database?
  releaseLoggerEnv loggerEnv
  disconnectHedis hedisEnv

type Env = EnvR AppEnv

type FlowHandler = FlowHandlerR AppEnv

type FlowServer api = FlowServerR AppEnv api

type Flow = FlowR AppEnv

instance Registry Flow where
  registryLookup registryUrl = Registry.withSubscriberCache $ Registry.registryLookup registryUrl

cacheRegistryKey :: Text
cacheRegistryKey = "dynamic-offer-driver-app:registry:"

instance Cache Subscriber Flow where
  type CacheKey Subscriber = SimpleLookupRequest
  getKey = Redis.get . (cacheRegistryKey <>) . lookupRequestToRedisKey
  setKey = Redis.set . (cacheRegistryKey <>) . lookupRequestToRedisKey
  delKey = Redis.del . (cacheRegistryKey <>) . lookupRequestToRedisKey

instance CacheEx Subscriber Flow where
  setKeyEx ttl = (\k v -> Redis.setExp k v ttl.getSeconds) . (cacheRegistryKey <>) . lookupRequestToRedisKey
