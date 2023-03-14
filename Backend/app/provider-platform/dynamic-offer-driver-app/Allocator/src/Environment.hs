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

import Data.String.Conversions (cs)
import "dynamic-offer-driver-app" Environment (AppCfg (..))
import Kernel.External.Encryption (EncTools)
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config
import Kernel.Storage.Hedis (HedisEnv, connectHedis, disconnectHedis)
import Kernel.Streaming.Kafka.Producer (buildKafkaProducerTools)
import Kernel.Streaming.Kafka.Producer.Types (KafkaProducerTools)
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
    maxParallelSearchRequests :: Int,
    kafkaProducerTools :: KafkaProducerTools,
    appPrefix :: Text
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
  kafkaProducerTools <- buildKafkaProducerTools kafkaProducerCfg
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
