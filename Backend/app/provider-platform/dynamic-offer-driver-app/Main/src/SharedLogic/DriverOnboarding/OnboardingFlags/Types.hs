-- | The effect constraints an onboarding flag recompute needs. Kept dependency-free so the
--   document-fetch layer can use it without an import cycle.
module SharedLogic.DriverOnboarding.OnboardingFlags.Types
  ( OnboardingFlow,
  )
where

import qualified AWS.S3 as S3
import Kernel.External.Types (SchedulerFlow, ServiceFlow)
import Kernel.Prelude
import Kernel.Sms.Config (SmsConfig)
import qualified Kernel.Storage.Clickhouse.Config as CH
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Commons (KafkaTopic)
import Kernel.Types.SlidingWindowLimiter (APIRateLimitOptions)
import Kernel.Utils.Common

type OnboardingFlow m r =
  ( MonadFlow m,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    CacheFlow m r,
    EncFlow m r,
    Redis.HedisFlow m r,
    Redis.HedisLTSFlowEnv r,
    ServiceFlow m r,
    HasFlowEnv m r '["smsCfg" ::: SmsConfig, "maxNotificationShards" ::: Int],
    HasField "broadcastMessageTopic" r KafkaTopic,
    HasField "authTokenCacheExpiry" r Seconds,
    HasField "s3Env" r (S3.S3Env m),
    HasField "externalServiceRateLimitOptions" r APIRateLimitOptions,
    HasField "imageExtractionTimeoutSec" r Seconds,
    SchedulerFlow r,
    HasField "blackListedJobs" r [Text],
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  )
