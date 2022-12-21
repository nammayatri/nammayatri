module Environment
  ( HandlerCfg (..),
    HandlerEnv (..),
    JobType (..),
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
import Beckn.Types.SlidingWindowCounters (SlidingWindowOptions)
import Beckn.Utils.Common
import Beckn.Utils.Dhall (FromDhall)
import Beckn.Utils.IOLogging
import Beckn.Utils.Servant.SignatureAuth
import Data.String.Conversions (cs)
import "driver-offer-bpp" Environment (AppCfg (..))
import Lib.Scheduler.Environment (SchedulerConfig (..))
import SharedLogic.Allocator (JobType (..))
import SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Config (SendSearchRequestJobConfig)
import SharedLogic.DriverPool (DriverPoolConfig)
import SharedLogic.GoogleTranslate
import Storage.CachedQueries.CacheConfig (CacheConfig)
import System.Environment (lookupEnv)
import Tools.Metrics

data HandlerCfg = HandlerCfg
  { schedulerConfig :: SchedulerConfig JobType,
    appCfg :: AppCfg
  }
  deriving (Generic, FromDhall)

data HandlerEnv = HandlerEnv
  { signingKey :: Base64,
    signatureExpiry :: Seconds,
    httpClientOptions :: HttpClientOptions,
    loggerEnv :: LoggerEnv,
    esqDBEnv :: EsqDBEnv,
    esqDBReplicaEnv :: EsqDBEnv,
    encTools :: EncTools,
    hedisEnv :: HedisEnv,
    cacheConfig :: CacheConfig,
    cacheTranslationConfig :: CacheTranslationConfig,
    windowOptions :: SlidingWindowOptions,
    googleTranslateUrl :: BaseUrl,
    googleTranslateKey :: Text,
    coreMetrics :: CoreMetricsContainer,
    driverPoolCfg :: DriverPoolConfig,
    sendSearchRequestJobCfg :: SendSearchRequestJobConfig,
    ssrMetrics :: SendSearchRequestToDriverMetricsContainer
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
  return HandlerEnv {..}

releaseHandlerEnv :: HandlerEnv -> IO ()
releaseHandlerEnv HandlerEnv {..} = do
  releaseLoggerEnv loggerEnv
  disconnectHedis hedisEnv

type Flow = FlowR HandlerEnv

instance AuthenticatingEntity HandlerEnv where
  getSigningKey = (.signingKey)
  getSignatureExpiry = (.signatureExpiry)
