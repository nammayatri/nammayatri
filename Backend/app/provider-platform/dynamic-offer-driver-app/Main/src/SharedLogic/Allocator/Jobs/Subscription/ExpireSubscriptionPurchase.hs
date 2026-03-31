{-# OPTIONS_GHC -Wno-orphans #-}

module SharedLogic.Allocator.Jobs.Subscription.ExpireSubscriptionPurchase
  ( expireSubscriptionPurchase,
  )
where

import qualified Domain.Types.SubscriptionPurchase as DSP
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Utils.Common
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import Lib.Scheduler
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import SharedLogic.Allocator (AllocatorJobType (..), ExpireSubscriptionPurchaseJobData (..))
import SharedLogic.Finance.Prepaid (activateNextQueuedPurchaseExpiry, handleSubscriptionExpiry)
import SharedLogic.Ride (makeSubscriptionRunningBalanceLockKey)
import Storage.Beam.SchedulerJob ()
import qualified Storage.Queries.SubscriptionPurchase as QSP

expireSubscriptionPurchase ::
  ( BeamFlow m r,
    MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    JobCreatorEnv r,
    HasField "schedulerType" r SchedulerType
  ) =>
  Job 'ExpireSubscriptionPurchase ->
  m ExecutionResult
expireSubscriptionPurchase Job {id = jobId, jobInfo} = withLogTag ("JobId-" <> jobId.getId) $ do
  let jobData = jobInfo.jobData
  logInfo $ "Processing subscription expiry for: " <> jobData.subscriptionPurchaseId.getId

  mbPurchase <- QSP.findByPrimaryKey jobData.subscriptionPurchaseId
  case mbPurchase of
    Nothing -> do
      logInfo $ "Subscription purchase not found: " <> jobData.subscriptionPurchaseId.getId
      pure Complete
    Just purchase -> do
      Redis.withWaitOnLockRedisWithExpiry (makeSubscriptionRunningBalanceLockKey purchase.ownerId) 60 60 $ do
        mbLatestPurchase <- QSP.findByPrimaryKey jobData.subscriptionPurchaseId
        case mbLatestPurchase of
          Nothing -> do
            logInfo $ "Subscription purchase not found after lock acquisition: " <> jobData.subscriptionPurchaseId.getId
            pure ()
          Just latestPurchase -> do
            when (latestPurchase.status == DSP.ACTIVE) $ do
              handleSubscriptionExpiry latestPurchase
              -- After expiry, activate the next queued purchase's expiry timer.
              mbActivated <- activateNextQueuedPurchaseExpiry latestPurchase.ownerId latestPurchase.ownerType
              whenJust mbActivated $ \(nextPurchaseId, expiry) -> do
                now <- getCurrentTime
                let delay = diffUTCTime expiry now
                createJobIn @_ @'ExpireSubscriptionPurchase
                  (Just latestPurchase.merchantId)
                  (Just latestPurchase.merchantOperatingCityId)
                  delay
                  $ ExpireSubscriptionPurchaseJobData
                    { subscriptionPurchaseId = nextPurchaseId
                    }
            when (latestPurchase.status /= DSP.ACTIVE) $
              logInfo $ "Skipping expiry for subscription " <> latestPurchase.id.getId <> " in terminal state: " <> show latestPurchase.status
            pure ()
      pure Complete
