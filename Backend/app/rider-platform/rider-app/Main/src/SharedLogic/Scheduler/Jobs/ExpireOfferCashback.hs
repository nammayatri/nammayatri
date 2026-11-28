module SharedLogic.Scheduler.Jobs.ExpireOfferCashback where

import Kernel.External.Types (SchedulerFlow)
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Finance.Core.Types as Finance
import qualified Lib.Finance.Storage.Beam.BeamFlow as FinanceBeamFlow
import Lib.Scheduler
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import SharedLogic.Finance.CashbackExpiry (ExpireOutcome (..), expireRideCashback)
import SharedLogic.JobScheduler
import Storage.Beam.Payment ()

-- | How long to wait before looking again at a cashback whose payout is in flight at the deadline.
processingRecheckAfter :: NominalDiffTime
processingRecheckAfter = 6 * 3600

-- | An in-flight payout normally resolves within a day; after this many rechecks the job
--   stops, logs for review, and leaves the cashback to the payout status check.
maxProcessingRechecks :: Int
maxProcessingRechecks = 8

executeExpireOfferCashbackJob ::
  ( CacheFlow m r,
    MonadFlow m,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    SchedulerFlow r,
    HasField "blackListedJobs" r [Text],
    FinanceBeamFlow.BeamFlow m r,
    Finance.HasActorInfo m r
  ) =>
  Job 'ExpireOfferCashback ->
  m ExecutionResult
executeExpireOfferCashbackJob Job {id, jobInfo, merchantId, merchantOperatingCityId} = withLogTag ("JobId-" <> id.getId) $ do
  let jobData = jobInfo.jobData
  now <- getCurrentTime
  if now < jobData.redeemBy
    then pure $ ReSchedule jobData.redeemBy
    else
      expireRideCashback jobData.personId jobData.rideId >>= \case
        StillProcessing
          | jobData.attempt < maxProcessingRechecks -> do
            -- a new job, since ReSchedule keeps the old job data and the attempt count must grow
            createJobIn @_ @'ExpireOfferCashback merchantId merchantOperatingCityId processingRecheckAfter (jobData {attempt = jobData.attempt + 1} :: ExpireOfferCashbackJobData)
            pure Complete
          | otherwise -> do
            logError $ "Cashback payout still in flight after " <> show maxProcessingRechecks <> " rechecks, not forfeited; ride=" <> jobData.rideId.getId
            pure Complete
        _ -> pure Complete
