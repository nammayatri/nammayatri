module Environment
  ( HandlerCfg (..),
    HandlerEnv (..),
    SchedulerConfig (..),
    Flow,
    buildHandlerEnv,
    releaseHandlerEnv,
  )
where

import Beckn.External.Encryption (EncTools)
import Beckn.Prelude
import Beckn.Storage.Esqueleto.Config
import Beckn.Storage.Hedis (HedisEnv, connectHedis, disconnectHedis)
import Beckn.Types.Base64 (Base64)
import Beckn.Types.Common
import Beckn.Types.Flow
import Beckn.Utils.Common
import Beckn.Utils.Dhall (FromDhall)
import Beckn.Utils.IOLogging
import Beckn.Utils.Servant.SignatureAuth
import Data.String.Conversions (cs)
import "driver-offer-bpp" Environment (AppCfg (..))
import Lib.Scheduler.Environment (SchedulerConfig (..))
import SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Config (SendSearchRequestJobConfig)
import SharedLogic.DriverPool (DriverPoolConfig, IntelligentPoolConfig, OverrideDriverPoolConfig, RideRequestPopupConfig)
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
    rideRequestPopupConfig :: RideRequestPopupConfig,
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
