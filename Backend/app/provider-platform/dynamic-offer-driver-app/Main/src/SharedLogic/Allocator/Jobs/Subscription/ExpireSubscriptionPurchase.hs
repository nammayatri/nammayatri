{-# OPTIONS_GHC -Wno-orphans #-}

module SharedLogic.Allocator.Jobs.Subscription.ExpireSubscriptionPurchase
  ( expireSubscriptionPurchase,
  )
where

import qualified Domain.Types.SubscriptionPurchase as DSP
import Kernel.Prelude
import qualified Kernel.Storage.Clickhouse.Config as CH
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Utils.Common
import qualified Lib.Finance.Core.Types as Finance
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import Lib.Scheduler
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import SharedLogic.Allocator (AllocatorJobType (..), ExpireSubscriptionPurchaseJobData (..))
import SharedLogic.Finance.Prepaid (activateNextQueuedPurchaseExpiry, handleSubscriptionExpiry, ownerHasRideInFlight, resolvePrepaidScope)
import SharedLogic.Ride (makeSubscriptionRunningBalanceLockKey)
import Storage.Beam.SchedulerJob ()
import qualified Storage.Queries.SubscriptionPurchase as QSP

rideInFlightRetryDelay :: NominalDiffTime
rideInFlightRetryDelay = 15 * 60

expireSubscriptionPurchase ::
  ( BeamFlow m r,
    MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    JobCreatorEnv r,
    HasField "schedulerType" r SchedulerType,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv,
    Finance.HasActorInfo m r
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
      rideInFlight <-
        if purchase.status == DSP.ACTIVE
          then ownerHasRideInFlight purchase.ownerType purchase.ownerId purchase.expiryDate
          else pure False
      if rideInFlight
        then do
          now <- getCurrentTime
          let retryAt = addUTCTime rideInFlightRetryDelay now
          logInfo $ "Subscription expiry deferred for " <> purchase.id.getId <> " (ride in progress); rescheduling to " <> show retryAt
          pure $ ReSchedule retryAt
        else do
          Redis.withWaitOnLockRedisWithExpiry (makeSubscriptionRunningBalanceLockKey purchase.ownerId) 10 10 $ do
            didExpire <- handleSubscriptionExpiry purchase
            when didExpire $ do
              prepaidScope <- resolvePrepaidScope purchase.merchantOperatingCityId purchase.vehicleCategory
              mbActivated <- activateNextQueuedPurchaseExpiry purchase.ownerId purchase.ownerType prepaidScope
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
          mbPurchaseAfter <- QSP.findByPrimaryKey purchase.id
          if maybe False (\p -> p.status == DSP.ACTIVE) mbPurchaseAfter
            then do
              now <- getCurrentTime
              let retryAt = addUTCTime rideInFlightRetryDelay now
              logInfo $ "Subscription expiry did not complete for " <> purchase.id.getId <> "; rescheduling to " <> show retryAt
              pure $ ReSchedule retryAt
            else pure Complete
