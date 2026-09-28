{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Manually-triggered sweep for referral payouts stranded on historical days.
--
-- The daily 'DriverReferralPayout' job only ever looks at a single
-- @merchant_local_date@ (@Se.Eq@ on an indexed column), so when its self-perpetuating
-- chain breaks, every day it missed stays unpaid forever. This job walks the range
-- @[cursorDate .. toDate]@ one day at a time, reusing the same indexed lookup and the
-- same 'callPayout' path, and terminates at the end of the range instead of chaining
-- forward indefinitely.
--
-- Each run drains at most @payoutBatchLimit@ rows and then enqueues its own
-- successor, so progress is visible row-by-row in @scheduler_job@.
module SharedLogic.Allocator.Jobs.Payout.DriverReferralPayoutBacklog where

import Data.Time (addDays)
import qualified Domain.Action.UI.Payout as DAP
import qualified Domain.Types.DailyStats as DS
import qualified Domain.Types.DriverInformation as DI
import Kernel.External.Types (SchedulerFlow)
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import Kernel.Types.Error
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getConfig, getOneConfig)
import qualified Lib.Finance.Core.Types as Finance
import Lib.Scheduler
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import SharedLogic.Allocator
import SharedLogic.Allocator.Jobs.Payout.DriverReferralPayout (callPayout)
import Storage.Beam.Payment ()
import Storage.Beam.SchedulerJob ()
import qualified Storage.CachedQueries.Merchant.PayoutConfig as CQPC
import Storage.ConfigPilot.Config.PayoutConfig (PayoutConfigDimensions (..))
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.DailyStats as QDailyStats
import qualified Storage.Queries.DailyStatsExtra as QDSE
import qualified Storage.Queries.DriverInformation as QDI

-- | Upper bound on batches attempted for a single @cursorDate@ before the sweep
-- gives up on that day and moves on. At a @payoutBatchLimit@ of 10 this is 500 rows,
-- far above any realistic single-day backlog, and it guarantees a row that never
-- leaves @statusForRetry@ cannot stall the whole sweep.
maxAttemptsPerCursorDate :: Int
maxAttemptsPerCursorDate = 50

-- | Delay before the next batch / next day is picked up.
backlogStepDelay :: NominalDiffTime
backlogStepDelay = 60

sendDriverReferralPayoutBacklogJobData ::
  ( EncFlow m r,
    CacheFlow m r,
    Finance.HasActorInfo m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    SchedulerFlow r,
    HasFlowEnv m r '["selfBaseUrl" ::: BaseUrl],
    HasKafkaProducer r,
    HasField "blackListedJobs" r [Text]
  ) =>
  Job 'DriverReferralPayoutBacklog ->
  m ExecutionResult
sendDriverReferralPayoutBacklogJobData Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
      merchantId = jobData.merchantId
      merchantOpCityId = jobData.merchantOperatingCityId
      statusForRetry = jobData.statusForRetry
      cursorDate = jobData.cursorDate
      toDate = jobData.toDate
      attempt = fromMaybe 0 jobData.cursorAttempt
  if cursorDate > toDate
    then do
      logInfo $ "REFERRAL_BACKLOG_SWEEP_DONE: city=" <> merchantOpCityId.getId <> " cursor=" <> show cursorDate <> " past toDate=" <> show toDate
      pure Complete
    else do
      payoutConfigList <-
        getConfig
          (PayoutConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId, vehicleCategory = Nothing, isPayoutEnabled = Just True})
          (Just (CQPC.findAllByMerchantOpCityId merchantOpCityId Nothing))
      -- Fail loudly rather than silently doing nothing: an empty list here is exactly the
      -- misconfiguration that kills the daily chain without leaving a trace.
      when (null payoutConfigList) $
        throwError $ InternalError ("REFERRAL_BACKLOG_NO_ENABLED_PAYOUT_CONFIG for city: " <> merchantOpCityId.getId)
      transporterConfig <-
        getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId}) Nothing
          >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
      dStatsForDay <- QDSE.findAllByDateAndPayoutStatus (Just transporterConfig.payoutBatchLimit) (Just 0) cursorDate statusForRetry merchantOpCityId
      if null dStatsForDay
        then advanceCursor merchantId merchantOpCityId jobData cursorDate toDate
        else
          if attempt >= maxAttemptsPerCursorDate
            then do
              logError $
                "REFERRAL_BACKLOG_CURSOR_STUCK: city=" <> merchantOpCityId.getId <> " date=" <> show cursorDate
                  <> " still has "
                  <> show (length dStatsForDay)
                  <> " row(s) in "
                  <> show statusForRetry
                  <> " after "
                  <> show attempt
                  <> " attempts; skipping to next day"
              advanceCursor merchantId merchantOpCityId jobData cursorDate toDate
            else do
              mapM_ (updateManualStatus transporterConfig) dStatsForDay
              let totalPayoutCount ds = ds.referralCounts + ds.d2dReferralCounts
                  dStatsList = filter (\ds -> totalPayoutCount ds <= transporterConfig.maxPayoutReferralForADay) dStatsForDay
              statsWithVpaList <- mapM getStatsWithVpa dStatsList
              let payable =
                    filter
                      (\(_, dInfo) -> isJust dInfo.payoutVpa && dInfo.payoutVpaStatus /= Just DI.MANUALLY_ADDED && dInfo.isBlockedForReferralPayout /= Just True)
                      statsWithVpaList
              logInfo $
                "REFERRAL_BACKLOG_BATCH: city=" <> merchantOpCityId.getId <> " date=" <> show cursorDate
                  <> " attempt="
                  <> show attempt
                  <> " found="
                  <> show (length dStatsForDay)
                  <> " payable="
                  <> show (length payable)
              for_ payable $ \(ds, dInfo) ->
                fork ("backlog payout for driverId: " <> ds.driverId.getId) $
                  callPayout ds dInfo dInfo.payoutVpa payoutConfigList statusForRetry
              -- Same day, next batch.
              enqueue merchantId merchantOpCityId jobData {cursorAttempt = Just (attempt + 1)}
              pure Complete
  where
    enqueue merchantId merchantOpCityId newJobData =
      Redis.runInMasterCloudRedisCell $
        createJobIn @_ @'DriverReferralPayoutBacklog (Just merchantId) (Just merchantOpCityId) backlogStepDelay newJobData

    advanceCursor merchantId merchantOpCityId jobData cursorDate toDate = do
      let nextDate = addDays 1 cursorDate
      if nextDate > toDate
        then do
          logInfo $ "REFERRAL_BACKLOG_SWEEP_DONE: city=" <> merchantOpCityId.getId <> " finished at " <> show cursorDate
          pure Complete
        else do
          logInfo $ "REFERRAL_BACKLOG_ADVANCE: city=" <> merchantOpCityId.getId <> " " <> show cursorDate <> " -> " <> show nextDate
          enqueue merchantId merchantOpCityId jobData {cursorDate = nextDate, cursorAttempt = Just 0}
          pure Complete

    -- Mirrors the daily job: rows that can never be paid are moved out of
    -- statusForRetry here, which is what lets the per-day loop drain.
    getStatsWithVpa dStats = do
      dInfo <- QDI.findById dStats.driverId >>= fromMaybeM (PersonNotFound dStats.driverId.getId)
      when (isNothing dInfo.payoutVpa || dInfo.payoutVpaStatus == Just DI.MANUALLY_ADDED) $
        updatePayoutStatus DS.PendingForVpa dStats
      when (dInfo.isBlockedForReferralPayout == Just True) $
        updatePayoutStatus DS.ManualReview dStats
      pure (dStats, dInfo)

    updateManualStatus transporterConfig dStats =
      when ((dStats.referralCounts + dStats.d2dReferralCounts) > transporterConfig.maxPayoutReferralForADay) $
        updatePayoutStatus DS.ManualReview dStats

    updatePayoutStatus status dStats =
      Redis.withWaitOnLockRedisWithExpiry (DAP.payoutProcessingLockKey dStats.driverId.getId) 1 1 $
        QDailyStats.updatePayoutStatusById status dStats.id
