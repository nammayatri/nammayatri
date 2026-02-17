{-# OPTIONS_GHC -Wno-orphans #-}

module SharedLogic.Allocator.Jobs.Subscription.ExpireSubscriptionPurchase
  ( expireSubscriptionPurchase,
  )
where

import Kernel.Prelude
import Kernel.Utils.Common
import Lib.Scheduler
import SharedLogic.Allocator (AllocatorJobType (..))
import SharedLogic.Finance.Prepaid (handleSubscriptionExpiry)
import qualified Storage.Queries.SubscriptionPurchase as QSP

expireSubscriptionPurchase ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r
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
      pure Complete
