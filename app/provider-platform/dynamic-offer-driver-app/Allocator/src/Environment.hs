module Environment
  ( HandlerCfg (..),
    HandlerEnv (..),
    SchedulerConfig (..),
    Flow,
    buildHandlerEnv,
    releaseHandlerEnv,
  )
where

import Data.String.Conversions (cs)
import "driver-offer-bpp" Environment (AppCfg (..))
import Kernel.External.Encryption (EncTools)
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config
import Kernel.Storage.Hedis (HedisEnv, connectHedis, disconnectHedis)
import Kernel.Types.Base64 (Base64)
import Kernel.Types.Common
import Kernel.Types.Flow
import Kernel.Utils.Common
import Kernel.Utils.Dhall (FromDhall)
import Kernel.Utils.IOLogging
import Kernel.Utils.Servant.SignatureAuth
import Lib.Scheduler.Environment (SchedulerConfig (..))
import SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Config (SendSearchRequestJobConfig)
import SharedLogic.DriverPool (CancellationScoreRelatedConfig, DriverPoolConfig, IntelligentPoolConfig, OverrideDriverPoolConfig)
import SharedLogic.GoogleTranslate
import Storage.CachedQueries.CacheConfig (CacheConfig)
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
    cacheConfig :: CacheConfig,
    cacheTranslationConfig :: CacheTranslationConfig,
    googleTranslateUrl :: BaseUrl,
    googleTranslateKey :: Text,
    coreMetrics :: CoreMetricsContainer,
    intelligentPoolConfig :: IntelligentPoolConfig,
    driverPoolCfg :: DriverPoolConfig,
    defaultPopupDelay :: Seconds,
    cancellationScoreRelatedConfig :: CancellationScoreRelatedConfig,
    overrideDriverPoolConfig :: [OverrideDriverPoolConfig],
    sendSearchRequestJobCfg :: SendSearchRequestJobConfig,
    ssrMetrics :: SendSearchRequestToDriverMetricsContainer,
    maxParallelSearchRequests :: Int
  }
  deriving (Generic)

buildHandlerEnv :: HandlerCfg -> IO HandlerEnv
buildHandlerEnv HandlerCfg {..} = do
  let AppCfg {..} = appCfg
  hostname <- fmap cs <$> lookupEnv "POD_NAME" :: IO (Maybe Text)
  loggerEnv <- prepareLoggerEnv appCfg.loggerConfig hostname
  esqDBEnv <- prepareEsqDBEnv appCfg.esqDBCfg loggerEnv
  esqDBReplicaEnv <- prepareEsqDBEnv appCfg.esqDBReplicaCfg loggerEnv
  hedisEnv <- connectHedis appCfg.hedisCfg ("driver-offer-allocator:" <>)
  ssrMetrics <- registerSendSearchRequestToDriverMetricsContainer
  coreMetrics <- registerCoreMetricsContainer
  let overrideDriverPoolConfig = fromMaybe [] overrideDriverPoolCfg
  return HandlerEnv {..}

releaseHandlerEnv :: HandlerEnv -> IO ()
releaseHandlerEnv HandlerEnv {..} = do
  releaseLoggerEnv loggerEnv
  disconnectHedis hedisEnv

type Flow = FlowR HandlerEnv

instance AuthenticatingEntity HandlerEnv where
  getSigningKey = (.signingKey)
  getSignatureExpiry = (.signatureExpiry)
