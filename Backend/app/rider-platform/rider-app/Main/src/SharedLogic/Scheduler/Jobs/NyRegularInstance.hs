{-# LANGUAGE FlexibleContexts #-}

module SharedLogic.Scheduler.Jobs.NyRegularInstance where

import qualified Data.HashMap.Strict as HM
import qualified Domain.Types.NyRegularInstanceLog as NyRegularInstanceLog
import Kernel.External.Encryption (EncFlow)
import Kernel.External.Slack.Types (SlackConfig)
import Kernel.Prelude
import Kernel.Storage.Clickhouse.Config (ClickhouseFlow)
import Kernel.Storage.Esqueleto.Config (EsqDBFlow, EsqDBReplicaFlow)
import Kernel.Streaming.Kafka.Producer.Types (KafkaProducerTools)
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.App (HasFlowEnv, MonadFlow)
import Kernel.Types.Common (Seconds, getCurrentTime)
import Kernel.Types.SlidingWindowLimiter
import Kernel.Types.Version (CloudType)
import Kernel.Utils.Common (CacheFlow, type (:::))
import Kernel.Utils.Logging (logError, logInfo, withLogTag)
import Kernel.Utils.Servant.Client (RetryCfg)
import Lib.Scheduler (ExecutionResult (..), Job (..))
import Lib.SessionizerMetrics.Types.Event (EventStreamFlow)
import SharedLogic.JobScheduler (RiderJobType (..))
import SharedLogic.NyRegularSubscriptionHasher (calculateSubscriptionSchedulingHash)
import qualified SharedLogic.NySubscription as NySubscription
import qualified Storage.Queries.NyRegularInstanceLog as NyRegularInstanceLog
import qualified Storage.Queries.NyRegularSubscription as QNyRegularSubscription
import Tools.Metrics.BAPMetrics.Types
import Tools.Metrics.BAPMetrics.Types ()
import TransactionLogs.Types

runNyRegularInstanceJob ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    EventStreamFlow m r,
    ClickhouseFlow m r,
    HasBAPMetrics m r,
    HasFlowEnv m r '["nyGatewayUrl" ::: BaseUrl],
    HasFlowEnv m r '["ondcGatewayUrl" ::: BaseUrl],
    HasFlowEnv m r '["searchRequestExpiry" ::: Maybe Seconds],
    HasFlowEnv m r '["collectRouteData" ::: Bool],
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasField "hotSpotExpiry" r Seconds,
    HasFlowEnv m r '["ondcTokenHashMap" ::: HM.HashMap KeyConfig TokenConfig],
    HasFlowEnv m r '["slackCfg" ::: SlackConfig],
    HasFlowEnv m r '["searchRateLimitOptions" ::: APIRateLimitOptions],
    HasFlowEnv m r '["searchLimitExceedNotificationTemplate" ::: Text],
    MonadFlow m,
    CoreMetrics m,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasField "shortDurationRetryCfg" r RetryCfg,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasFlowEnv m r '["cloudType" ::: Maybe CloudType]
  ) =>
  Job 'NyRegularInstance ->
  m ExecutionResult
runNyRegularInstanceJob Job {id = jobId, jobInfo} = withLogTag ("JobId-" <> jobId.getId) do
  let jobDataDetails = jobInfo.jobData
      subscriptionId = jobDataDetails.nyRegularSubscriptionId
      jobScheduledTime = jobDataDetails.scheduledTime
      expectedHash = jobDataDetails.expectedSchedulingHash

  logInfo $ "Running NyRegularInstance job " <> show jobId <> " for subscription " <> show subscriptionId <> " scheduled at " <> show jobScheduledTime

  mCurrentSubscription <- QNyRegularSubscription.findById subscriptionId

  case mCurrentSubscription of
    Nothing -> do
      logError $ "Subscription " <> show subscriptionId <> " not found during execution of job " <> show jobId <> ". Terminating job."
      return $ Terminate "Subscription not found during job execution"
    Just currentSubscription -> do
      currentLiveHash <- calculateSubscriptionSchedulingHash currentSubscription

      if (show currentLiveHash) /= expectedHash
        then do
          logInfo $
            "Terminating NyRegularInstance job " <> show jobId
              <> ". Subscription "
              <> show subscriptionId
              <> " scheduling state changed. Expected hash: "
              <> show expectedHash
              <> ", current live hash: "
              <> show currentLiveHash
              <> "."
          return $ Terminate "Subscription scheduling state changed"
        else do
          logInfo $ "Subscription " <> show subscriptionId <> " state is consistent for job " <> show jobId <> ". Proceeding."
          searchId <- NySubscription.triggerSubscriptionSearch currentSubscription
          now <- getCurrentTime
          void . NyRegularInstanceLog.create $
            NyRegularInstanceLog.NyRegularInstanceLog
              { nyRegularSubscriptionId = currentSubscription.id,
                automationStatus = NyRegularInstanceLog.SEARCH_SENT,
                instanceTransactionId = searchId.getId,
                scheduledPickupTime = jobScheduledTime,
                createdAt = now,
                updatedAt = now,
                merchantId = currentSubscription.merchantId,
                merchantOperatingCityId = currentSubscription.merchantOperatingCityId
              }
          pure Complete
