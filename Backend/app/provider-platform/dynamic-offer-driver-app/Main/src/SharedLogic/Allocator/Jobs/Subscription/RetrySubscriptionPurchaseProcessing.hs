{-# OPTIONS_GHC -Wno-orphans #-}

module SharedLogic.Allocator.Jobs.Subscription.RetrySubscriptionPurchaseProcessing
  ( retrySubscriptionPurchaseProcessing,
  )
where

import qualified Domain.Action.UI.Payment as PaymentAction
import qualified Domain.Types.SubscriptionPurchase as DSP
import Kernel.Prelude
import qualified Kernel.Storage.Clickhouse.Config as CH
import Kernel.Storage.Esqueleto (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common
import qualified Lib.Finance.Core.Types as Finance
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import Lib.Scheduler
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import SharedLogic.Allocator (AllocatorJobType (..), RetrySubscriptionPurchaseProcessingJobData (..))
import Storage.Beam.SchedulerJob ()
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.SubscriptionPurchase as QSP
import Tools.Error

retrySubscriptionPurchaseProcessing ::
  ( BeamFlow m r,
    MonadFlow m,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    CacheFlow m r,
    EncFlow m r,
    Redis.HedisFlow m r,
    JobCreatorEnv r,
    Redis.HedisLTSFlowEnv r,
    HasField "schedulerType" r SchedulerType,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv,
    Finance.HasActorInfo m r
  ) =>
  Job 'RetrySubscriptionPurchaseProcessing ->
  m ExecutionResult
retrySubscriptionPurchaseProcessing Job {id = jobId, jobInfo} = withLogTag ("JobId-" <> jobId.getId) $ do
  let jobData = jobInfo.jobData
  logInfo $ "Retrying subscription purchase processing for: " <> jobData.subscriptionPurchaseId.getId <> " (attempt " <> show (jobData.currentRetry + 1) <> "/" <> show jobData.maxRetries <> ")"
  mbPurchase <- QSP.findByPrimaryKey jobData.subscriptionPurchaseId
  case mbPurchase of
    Nothing -> do
      logInfo $ "Subscription purchase not found: " <> jobData.subscriptionPurchaseId.getId
      pure Complete
    Just purchase -> do
      if purchase.status /= DSP.PENDING
        then do
          logInfo $ "Subscription purchase already processed (status: " <> show purchase.status <> "): " <> purchase.id.getId
          pure Complete
        else do
          person <- QP.findById (Id purchase.ownerId) >>= fromMaybeM (PersonDoesNotExist purchase.ownerId)
          PaymentAction.processSubscriptionPurchasePayment purchase.merchantId person purchase
          latestPurchase <- QSP.findByPrimaryKey purchase.id >>= fromMaybeM (InternalError $ "Subscription purchase not found after processing: " <> purchase.id.getId)
          if latestPurchase.status == DSP.ACTIVE
            then do
              logInfo $ "Subscription purchase successfully processed: " <> purchase.id.getId
              pure Complete
            else do
              let nextRetry = jobData.currentRetry + 1
              if nextRetry >= jobData.maxRetries
                then do
                  logError $ "Subscription purchase processing exhausted all " <> show jobData.maxRetries <> " retries: " <> purchase.id.getId
                  pure $ Terminate "Max retries exceeded for subscription purchase processing"
                else do
                  logInfo $ "Subscription purchase still pending, scheduling retry " <> show (nextRetry + 1) <> "/" <> show jobData.maxRetries <> ": " <> purchase.id.getId
                  createJobIn @_ @'RetrySubscriptionPurchaseProcessing
                    (Just purchase.merchantId)
                    (Just purchase.merchantOperatingCityId)
                    (fromIntegral jobData.retryDelaySec)
                    $ RetrySubscriptionPurchaseProcessingJobData
                      { subscriptionPurchaseId = purchase.id,
                        maxRetries = jobData.maxRetries,
                        currentRetry = nextRetry,
                        retryDelaySec = jobData.retryDelaySec
                      }
                  pure Complete
