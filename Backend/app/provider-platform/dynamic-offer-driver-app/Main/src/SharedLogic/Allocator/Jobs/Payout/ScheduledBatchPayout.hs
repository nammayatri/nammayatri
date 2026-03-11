{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Allocator.Jobs.Payout.ScheduledBatchPayout (sendScheduledBatchPayout) where

import qualified Data.Time as Time
import qualified Data.Time.Calendar.WeekDate as Time
import Domain.Action.UI.DriverWallet
  ( PayoutContext (..),
    counterpartyFromRole,
    initiateWalletPayout,
    resolvePayoutVpa,
  )
import Domain.Action.UI.Ride.EndRide.Internal (makeWalletRunningBalanceLockKey)
import qualified Domain.Types.DriverInformation as DI
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.ScheduledPayoutConfig as DSPC
import qualified Domain.Types.TransporterConfig as DTConf
import Kernel.External.Types (SchedulerFlow)
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Payment.Domain.Types.Common as DPayment
import qualified Lib.Payment.Domain.Types.PayoutRequest as PR
import Lib.Scheduler
import SharedLogic.Allocator
import SharedLogic.Finance.Wallet
import Storage.Beam.Payment ()
import Storage.Beam.SchedulerJob ()
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.DriverInformationExtra as QDIE
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.ScheduledPayoutConfig as QSPC

--------------------------------------------------------------------------------
-- Job entry point
--------------------------------------------------------------------------------

sendScheduledBatchPayout ::
  ( EncFlow m r,
    CacheFlow m r,
    MonadFlow m,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    SchedulerFlow r,
    BeamFlow m r,
    HasFlowEnv m r '["selfBaseUrl" ::: BaseUrl],
    HasKafkaProducer r,
    HasField "blackListedJobs" r [Text]
  ) =>
  Job 'ScheduledBatchPayout ->
  m ExecutionResult
sendScheduledBatchPayout Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
      merchantOpCityId = jobData.merchantOperatingCityId
      category = jobData.payoutCategory

  -- Load config
  mbConfig <- QSPC.findByMerchantOpCityIdAndCategory merchantOpCityId category
  case mbConfig of
    Nothing -> do
      logWarning $ "No ScheduledPayoutConfig found for " <> show category <> " in city " <> merchantOpCityId.getId
      pure Complete
    Just config ->
      if not config.isEnabled
        then do
          logInfo $ "Scheduled payout disabled for " <> show category
          pure Complete
        else processCategory config jobData

--------------------------------------------------------------------------------
-- Category dispatch
--------------------------------------------------------------------------------

processCategory ::
  ( EncFlow m r,
    CacheFlow m r,
    MonadFlow m,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    SchedulerFlow r,
    BeamFlow m r,
    HasFlowEnv m r '["selfBaseUrl" ::: BaseUrl],
    HasKafkaProducer r,
    HasField "blackListedJobs" r [Text]
  ) =>
  DSPC.ScheduledPayoutConfig ->
  ScheduledBatchPayoutJobData ->
  m ExecutionResult
processCategory config jobData = do
  case config.payoutCategory of
    DPayment.DRIVER_WALLET_TRANSACTION -> processWalletPayouts config jobData
    DPayment.DRIVER_DAILY_STATS -> do
      logInfo "REFERRAL: not yet implemented in unified framework. Use the legacy DriverReferralPayout job."
      pure Complete
    DPayment.SPECIAL_ZONE_PAYOUT -> do
      logInfo "SPECIAL_ZONE: not yet implemented in unified framework."
      pure Complete
    other -> do
      logWarning $ "Unsupported payout category for scheduled batch: " <> show other
      pure Complete

--------------------------------------------------------------------------------
-- Wallet payout handler (reuses DriverWallet.hs helpers)
--------------------------------------------------------------------------------

processWalletPayouts ::
  ( EncFlow m r,
    CacheFlow m r,
    MonadFlow m,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    SchedulerFlow r,
    BeamFlow m r,
    HasFlowEnv m r '["selfBaseUrl" ::: BaseUrl],
    HasKafkaProducer r,
    HasField "blackListedJobs" r [Text]
  ) =>
  DSPC.ScheduledPayoutConfig ->
  ScheduledBatchPayoutJobData ->
  m ExecutionResult
processWalletPayouts config jobData = do
  let merchantId = jobData.merchantId
      merchantOpCityId = jobData.merchantOperatingCityId
  transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  let walletEnabled = fromMaybe False merchant.prepaidSubscriptionAndWalletEnabled && transporterConfig.driverWalletConfig.enableWalletPayout
  if not walletEnabled
    then do
      logInfo "Wallet payouts disabled at transporter level"
      pure Complete
    else do
      -- Cursor-based pagination: use lastDriverId from Redis
      let cursorKey = "ScheduledBatchPayout:Cursor:" <> merchantOpCityId.getId <> ":" <> show config.payoutCategory
      mbLastDriverId <- Redis.get cursorKey
      eligibleDriverInfos <- QDIE.findEligibleForScheduledPayout merchantOpCityId config.batchSize mbLastDriverId
      if null eligibleDriverInfos
        then do
          -- All drivers processed; clear cursor and schedule for next interval
          Redis.del cursorKey
          nextTime <- computeNextRunTime config
          logInfo $ "All drivers processed. Next run at: " <> show nextTime
          pure $ ReSchedule nextTime
        else do
          -- Track the last driver ID for cursor
          let lastDriverId = (.driverId) $ last eligibleDriverInfos
          Redis.setExp cursorKey lastDriverId 86400 -- TTL: 1 day
          for_ eligibleDriverInfos $ \driverInfo -> do
            fork ("ScheduledWalletPayout:Driver:" <> driverInfo.driverId.getId) $ do
              processOneWalletPayout config transporterConfig merchantId merchantOpCityId driverInfo
          -- Short gap reschedule for next batch
          nextBatch <- addUTCTime 5 <$> getCurrentTime
          pure $ ReSchedule nextBatch

-- | Process a single driver's wallet payout.
--   Reuses PayoutContext and helpers from DriverWallet module.
processOneWalletPayout ::
  ( EncFlow m r,
    CacheFlow m r,
    MonadFlow m,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    BeamFlow m r,
    HasFlowEnv m r '["selfBaseUrl" ::: BaseUrl],
    HasKafkaProducer r
  ) =>
  DSPC.ScheduledPayoutConfig ->
  DTConf.TransporterConfig ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  DI.DriverInformation ->
  m ()
processOneWalletPayout config transporterConfig merchantId merchantOpCityId driverInfo = do
  let driverId = driverInfo.driverId
  result <- try $ do
    person <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
    let counterparty = counterpartyFromRole person.role
        ctx =
          PayoutContext
            { driverId = driverId,
              merchantId = merchantId,
              mocId = merchantOpCityId,
              person = person,
              driverInfo = driverInfo,
              transporterConfig = transporterConfig
            }

    Redis.withWaitOnLockRedisWithExpiry (makeWalletRunningBalanceLockKey driverId.getId) 10 10 $ do
      now <- getCurrentTime
      mbAccount <- getWalletAccountByOwner counterparty driverId.getId
      let mbAccountId = (.id) <$> mbAccount
      walletBalance <- fromMaybe 0 <$> getWalletBalanceByOwner counterparty driverId.getId
      -- Single query: get both non-redeemable balance and redeemable entry IDs
      let timeDiff = secondsToNominalDiffTime transporterConfig.timeDiffFromUtc
          cutOffDays = transporterConfig.driverWalletConfig.payoutCutOffDays
          cutoff = payoutCutoffTimeUTC timeDiff cutOffDays now
      (nonRedeemable, redeemableIds) <- case mbAccountId of
        Nothing -> pure (0, [])
        Just accountId -> getPayoutEligibilityData accountId cutoff now
      let payoutableBalance = walletBalance - nonRedeemable

      when (payoutableBalance >= config.minimumPayoutAmount) $ do
        vpa <- resolvePayoutVpa ctx
        -- Skip manually-added VPAs
        when (driverInfo.payoutVpaStatus /= Just DI.MANUALLY_ADDED) $ do
          initiateWalletPayout ctx vpa payoutableBalance PR.SCHEDULED Nothing (Just cutoff) (map (.getId) redeemableIds)
  case result of
    Left (e :: SomeException) -> logError $ "ScheduledWalletPayout error for driver " <> driverId.getId <> ": " <> show e
    Right _ -> pure ()

--------------------------------------------------------------------------------
-- Next run time computation
--------------------------------------------------------------------------------

computeNextRunTime ::
  (MonadFlow m) =>
  DSPC.ScheduledPayoutConfig ->
  m UTCTime
computeNextRunTime config = do
  now <- getCurrentTime
  let timeDiff = secondsToNominalDiffTime config.timeDiffFromUtc
      localNow = Time.addUTCTime timeDiff now
      localDay = Time.utctDay localNow
      (timeHours, timeMinutes) = parseTimeOfDay config.timeOfDay
      targetTimeOfDay = Time.timeOfDayToTime (Time.TimeOfDay timeHours timeMinutes 0)
  case config.frequency of
    DSPC.DAILY -> do
      let nextLocalDay =
            if Time.utctDayTime localNow > targetTimeOfDay
              then Time.addDays 1 localDay
              else localDay
      pure $ Time.addUTCTime (negate timeDiff) (Time.UTCTime nextLocalDay targetTimeOfDay)
    DSPC.WEEKLY -> do
      let targetDow = fromMaybe 1 config.dayOfWeek -- 1=Monday
          (_, _, currentDow) = Time.toWeekDate localDay
          daysUntil =
            let diff = targetDow - currentDow
             in if diff < 0 || (diff == 0 && Time.utctDayTime localNow > targetTimeOfDay)
                  then diff + 7
                  else if diff == 0 then 0 else diff
          nextLocalDay = Time.addDays (fromIntegral daysUntil) localDay
      pure $ Time.addUTCTime (negate timeDiff) (Time.UTCTime nextLocalDay targetTimeOfDay)
    DSPC.MONTHLY -> do
      let targetDom = fromMaybe 1 config.dayOfMonth
          (year, month, currentDom) = Time.toGregorian localDay
          nextDate =
            if currentDom < targetDom || (currentDom == targetDom && Time.utctDayTime localNow <= targetTimeOfDay)
              then Time.fromGregorian year month (min targetDom 28)
              else
                let (nextYear, nextMonth) =
                      if month == 12 then (year + 1, 1) else (year, month + 1)
                 in Time.fromGregorian nextYear nextMonth (min targetDom 28)
      pure $ Time.addUTCTime (negate timeDiff) (Time.UTCTime nextDate targetTimeOfDay)

-- | Parse "HH:MM" into (hours, minutes). Defaults to (2, 0) on failure.
parseTimeOfDay :: Text -> (Int, Int)
parseTimeOfDay t =
  case break (== ':') (toString t) of
    (hh, ':' : mm) -> case (readMaybe hh, readMaybe mm) of
      (Just h, Just m) -> (h, m)
      _ -> (2, 0)
    _ -> (2, 0)
