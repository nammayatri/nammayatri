{-# OPTIONS_GHC -Wno-orphans #-}

module SharedLogic.Allocator.Jobs.Subscription.ExpireSubscriptionPurchase
  ( expireSubscriptionPurchase,
  )
where

import qualified Domain.Types.SubscriptionPurchase as DSP
import Kernel.Prelude
import Kernel.Utils.Common
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import Lib.Scheduler
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import SharedLogic.Allocator (AllocatorJobType (..), ExpireSubscriptionPurchaseJobData (..))
import SharedLogic.Finance.Prepaid (activateNextQueuedPurchaseExpiry, handleSubscriptionExpiry)
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
      handleSubscriptionExpiry purchase
      -- After expiry, activate the next queued purchase's expiry timer (deferred FIFO)
      when (purchase.status == DSP.ACTIVE) $ do
        mbActivated <- activateNextQueuedPurchaseExpiry purchase.ownerId purchase.ownerType
        whenJust mbActivated $ \(nextPurchaseId, expiry) -> do
          now <- getCurrentTime
          let delay = diffUTCTime expiry now
          createJobIn @_ @'ExpireSubscriptionPurchase
            (Just purchase.merchantId)
            (Just purchase.merchantOperatingCityId)
            delay
            $ ExpireSubscriptionPurchaseJobData
              { subscriptionPurchaseId = nextPurchaseId
              }
      pure Complete
