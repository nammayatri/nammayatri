{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Allocator.Jobs.Payout.ScheduledBatchPayout
  ( sendScheduledBatchPayout,
    computeNextRunTime,
  )
where

import Data.Either (rights)
import qualified Data.Time as Time
import qualified Data.Time.Calendar.WeekDate as Time
import qualified Domain.Action.UI.Payout as UIPayout
import qualified Domain.Types.DriverInformation as DI
import Domain.Types.Extra.Plan
import qualified Domain.Types.FleetOwnerInformation as DFOI
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantServiceConfig as DEMSC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.ScheduledPayoutConfig as DSPC
import qualified Domain.Types.TransporterConfig as DTConf
import qualified Kernel.External.Payout.Interface as Payout
import Kernel.External.Types (SchedulerFlow, ServiceFlow)
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified Lib.Finance.AccountConfig as FinanceAccountConfig
import qualified Lib.Finance.Core.Types as Finance
import qualified Lib.Finance.Domain.Types.Account as FinanceAccount
import qualified Lib.Finance.Domain.Types.ScheduledTransferRule as STR
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Finance.Storage.Queries.ScheduledTransferRule as QSTR
import qualified Lib.Finance.Transfer.Scheduled as KernelSweep
import qualified Lib.Payment.Domain.Types.Common as DPayment
import qualified Lib.Payment.Domain.Types.PayoutBatch as DPayoutBatch
import qualified Lib.Payment.Domain.Types.PayoutRequest as PR
import qualified Lib.Payment.Storage.Beam.BeamFlow as PaymentBeamFlow
import Lib.Scheduler
import SharedLogic.Allocator
import SharedLogic.Finance.WalletPayout (PayoutContext (..), WalletPayoutParams (..), runWalletPayout)
import SharedLogic.Payout.Bulk.Driver (runBulkPayoutCycle)
import SharedLogic.Payout.Bulk.Eligibility (checkBulkPayoutEligibility)
import Storage.Beam.Payment ()
import Storage.Beam.SchedulerJob ()
import qualified Storage.CachedQueries.Merchant as CQM
import Storage.ConfigPilot.Config.ScheduledPayoutConfig (ScheduledPayoutConfigDimensions (..))
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.DriverInformationExtra as QDIE
import qualified Storage.Queries.FleetOwnerInformationExtra as QFOIE
import qualified Storage.Queries.Person as QPerson
import qualified Tools.Payout as TPayout

sendScheduledBatchPayout ::
  ( UIPayout.PayoutSettlementFlow m r,
    EncFlow m r,
    CacheFlow m r,
    Finance.HasActorInfo m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    SchedulerFlow r,
    ServiceFlow m r,
    BeamFlow m r,
    PaymentBeamFlow.BeamFlow m r,
    HasFlowEnv m r '["selfBaseUrl" ::: BaseUrl],
    HasKafkaProducer r,
    HasField "blackListedJobs" r [Text],
    Redis.HedisLTSFlowEnv r
  ) =>
  Job 'ScheduledBatchPayout ->
  m ExecutionResult
sendScheduledBatchPayout Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
      merchantOpCityId = jobData.merchantOperatingCityId
      category = jobData.payoutCategory

  -- Load config
  mbConfig <- getOneConfig (ScheduledPayoutConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId, isEnabled = Nothing, payoutCategory = Just category}) Nothing
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

processCategory ::
  ( UIPayout.PayoutSettlementFlow m r,
    EncFlow m r,
    CacheFlow m r,
    Finance.HasActorInfo m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    SchedulerFlow r,
    ServiceFlow m r,
    BeamFlow m r,
    PaymentBeamFlow.BeamFlow m r,
    HasFlowEnv m r '["selfBaseUrl" ::: BaseUrl],
    HasKafkaProducer r,
    HasField "blackListedJobs" r [Text],
    Redis.HedisLTSFlowEnv r
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

processWalletPayouts ::
  ( UIPayout.PayoutSettlementFlow m r,
    EncFlow m r,
    CacheFlow m r,
    Finance.HasActorInfo m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    SchedulerFlow r,
    ServiceFlow m r,
    BeamFlow m r,
    PaymentBeamFlow.BeamFlow m r,
    HasFlowEnv m r '["selfBaseUrl" ::: BaseUrl],
    HasKafkaProducer r,
    HasField "blackListedJobs" r [Text],
    Redis.HedisLTSFlowEnv r
  ) =>
  DSPC.ScheduledPayoutConfig ->
  ScheduledBatchPayoutJobData ->
  m ExecutionResult
processWalletPayouts config jobData = do
  let merchantId = jobData.merchantId
      merchantOpCityId = jobData.merchantOperatingCityId
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId}) Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  let walletEnabled = fromMaybe False merchant.prepaidSubscriptionAndWalletEnabled || transporterConfig.driverWalletConfig.enableWalletPayout
  if not walletEnabled
    then do
      logInfo "Wallet payouts disabled at transporter level"
      pure Complete
    else do
      -- Phase 3 (finance-kernel unification): the sweep SCHEDULE dual-reads from the
      -- kernel's finance_scheduled_transfer_rule, named by the (Liability, DRIVER)
      -- class config's autoTransferOut; batching/cursor/bulk mechanics are unchanged.
      -- Folding the page loop into Lib.Finance.Transfer.Scheduled.runScheduledSweep is
      -- the Phase-3b target — blocked on the bulk-vs-individual branching below, which
      -- the kernel sweep does not model.
      mbKernelRule <- findKernelSweepRule merchantId merchantOpCityId
      let effBatchSize = maybe config.batchSize (.batchSize) mbKernelRule
          effMinPayoutAmount = maybe config.minimumPayoutAmount (.minimumAmount) mbKernelRule
      let driverCursorKey = "ScheduledBatchPayout:Cursor:" <> merchantOpCityId.getId <> ":" <> show config.payoutCategory
          fleetCursorKey = "ScheduledBatchPayout:Cursor:Fleet:" <> merchantOpCityId.getId <> ":" <> show config.payoutCategory
          -- Bulk cycles that failed in a row, for the retry backoff.
          bulkFailuresKey = "ScheduledBatchPayout:BulkFailures:" <> merchantOpCityId.getId <> ":" <> show config.payoutCategory

      -- Process drivers
      mbLastDriverId <- Redis.get driverCursorKey
      (payoutServiceFlow, payoutServiceName) <- TPayout.getPayoutServiceFlowForMerchant (.createPayoutOrder) (TPayout.SubscriptionConfigOption PREPAID_SUBSCRIPTION) DEMSC.PayoutService merchantOpCityId
      let isPayoutVpaRequired = case payoutServiceFlow of
            Payout.JuspayFlow -> True
            Payout.StripeFlow -> False
            Payout.BulkFlow -> False -- HDFC CBX is account-and-IFSC based, not VPA based
      eligibleDriverInfos <- QDIE.findEligibleForScheduledPayout merchantOpCityId effBatchSize mbLastDriverId isPayoutVpaRequired
      unless (null eligibleDriverInfos) $ do
        let lastDriverId = (.driverId) $ last eligibleDriverInfos
        -- Bulk moves its cursor only after its cycle succeeds, so a failed cycle does not skip this page.
        unless (payoutServiceFlow == Payout.BulkFlow) $ Redis.setExp driverCursorKey lastDriverId 86400
        -- Bulk pays the whole page in one batch below, so it does not fork per driver.
        unless (payoutServiceFlow == Payout.BulkFlow) $
          for_ eligibleDriverInfos $ \driverInfo ->
            fork ("ScheduledWalletPayout:Driver:" <> driverInfo.driverId.getId) $
              processOneWalletPayout
                transporterConfig
                merchantId
                merchantOpCityId
                driverInfo.driverId
                driverInfo.payoutVpa
                (driverInfo.payoutVpaStatus == Just DI.MANUALLY_ADDED)
                effMinPayoutAmount

      -- Process fleet owners
      mbLastFleetId <- Redis.get fleetCursorKey
      eligibleFleetInfos <- QFOIE.findEligibleFleetOwnersForScheduledPayout merchantOpCityId effBatchSize mbLastFleetId isPayoutVpaRequired
      logDebug $
        "[SBP-DEBUG] mocId=" <> merchantOpCityId.getId
          <> " isPayoutVpaRequired="
          <> show isPayoutVpaRequired
          <> " minimumPayoutAmount="
          <> show effMinPayoutAmount
          <> " batchSize="
          <> show effBatchSize
          <> " kernelRule="
          <> show (mbKernelRule <&> (.name))
          <> " driverCursor="
          <> show mbLastDriverId
          <> " fleetCursor="
          <> show mbLastFleetId
          <> " eligibleDrivers="
          <> show (length eligibleDriverInfos)
          <> " eligibleFleetOwners="
          <> show (length eligibleFleetInfos)
          <> " driverIds="
          <> show (map (\d -> d.driverId.getId) eligibleDriverInfos)
          <> " fleetIds="
          <> show (map (\f -> f.fleetOwnerPersonId.getId) eligibleFleetInfos)
      unless (null eligibleFleetInfos) $ do
        let lastFleetId = (.fleetOwnerPersonId) $ last eligibleFleetInfos
        unless (payoutServiceFlow == Payout.BulkFlow) $ Redis.setExp fleetCursorKey lastFleetId 86400
        unless (payoutServiceFlow == Payout.BulkFlow) $
          for_ eligibleFleetInfos $ \fleetInfo ->
            fork ("ScheduledWalletPayout:Fleet:" <> fleetInfo.fleetOwnerPersonId.getId) $
              processOneWalletPayout
                transporterConfig
                merchantId
                merchantOpCityId
                fleetInfo.fleetOwnerPersonId
                fleetInfo.payoutVpa
                (fleetInfo.payoutVpaStatus == Just DFOI.MANUALLY_ADDED)
                effMinPayoutAmount

      -- Catch bulk errors here: this job is not retried on an exception, so one would end the city's sweep.
      bulkResult <-
        if payoutServiceFlow /= Payout.BulkFlow
          then pure (Right ())
          else try $ do
            -- One query for the page; anyone whose person row is gone is skipped.
            let pageIds = map (.driverId.getId) eligibleDriverInfos <> map (.fleetOwnerPersonId.getId) eligibleFleetInfos
            persons <- QPerson.findAllByPersonIds pageIds
            checks <- forM persons $ \person -> checkBulkPayoutEligibility config transporterConfig person
            let candidates = rights checks
            -- The city's status-check job settles the batch afterwards.
            unless (null candidates) $
              void $ runBulkPayoutCycle config payoutServiceName merchantId merchantOpCityId DPayoutBatch.SCHEDULED PR.SCHEDULED transporterConfig candidates
            unless (null eligibleDriverInfos) $
              Redis.setExp driverCursorKey ((.driverId) $ last eligibleDriverInfos) 86400
            unless (null eligibleFleetInfos) $
              Redis.setExp fleetCursorKey ((.fleetOwnerPersonId) $ last eligibleFleetInfos) 86400

      case bulkResult of
        Left (e :: SomeException) -> do
          -- Cursors stay on this page for the retry. Anyone already claimed has their money held,
          -- so the retry cannot pay them twice.
          failures <- Redis.incr bulkFailuresKey
          Redis.expire bulkFailuresKey 86400
          let retryInSeconds = min 3600 (30 * 2 ^ min 7 (failures - 1)) :: Integer
          logError $
            "ScheduledBatchPayout: bulk cycle failed for city " <> merchantOpCityId.getId
              <> " ("
              <> show failures
              <> " in a row); retrying this page in "
              <> show retryInSeconds
              <> " s: "
              <> show e
          Metrics.incrementGenericMetrics "scheduled_batch_payout_bulk_cycle_failed"
          ReSchedule . addUTCTime (fromIntegral retryInSeconds) <$> getCurrentTime
        Right () -> do
          when (payoutServiceFlow == Payout.BulkFlow) $ Redis.del bulkFailuresKey
          if null eligibleDriverInfos && null eligibleFleetInfos
            then do
              Redis.del driverCursorKey
              Redis.del fleetCursorKey
              nextTime <- case mbKernelRule of
                Just rule -> KernelSweep.computeNextRunTime rule <$> getCurrentTime
                Nothing -> computeNextRunTime config
              logInfo $ "All drivers and fleet owners processed. Next run at: " <> show nextTime
              pure $ ReSchedule nextTime
            else do
              nextBatch <- addUTCTime 5 <$> getCurrentTime
              pure $ ReSchedule nextBatch

-- | Process a single wallet payout for a driver or fleet owner.
--   Reuses PayoutContext and helpers from DriverWallet module.
processOneWalletPayout ::
  ( EncFlow m r,
    CacheFlow m r,
    Finance.HasActorInfo m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    BeamFlow m r,
    HasFlowEnv m r '["selfBaseUrl" ::: BaseUrl],
    HasKafkaProducer r,
    Redis.HedisLTSFlowEnv r,
    SchedulerFlow r,
    HasField "blackListedJobs" r [Text]
  ) =>
  DTConf.TransporterConfig ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Id DP.Person ->
  Maybe Text ->
  Bool -> -- isManuallyAdded
  HighPrecMoney -> -- minimum payout amount (kernel rule or legacy config)
  m ()
processOneWalletPayout transporterConfig merchantId merchantOpCityId personId mbPayoutVpa isManuallyAdded minPayoutAmount = do
  result <- try $
    -- Skip manually-added VPAs
    unless isManuallyAdded $ do
      person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
      let ctx =
            PayoutContext
              { driverId = personId,
                merchantId = merchantId,
                mocId = merchantOpCityId,
                person = person,
                payoutVpa = mbPayoutVpa,
                transporterConfig = transporterConfig
              }
          params =
            WalletPayoutParams
              { payoutType = PR.SCHEDULED,
                minimumPayoutAmount = minPayoutAmount,
                throwOnBelowMinimum = False
              }
      runWalletPayout ctx params
  case result of
    Left (e :: SomeException) -> logError $ "ScheduledWalletPayout error for " <> personId.getId <> ": " <> show e
    Right _ -> pure ()

-- | The kernel sweep rule for the city's (Liability, DRIVER) class, when the class
--   config names one and it is enabled. Nothing -> legacy ScheduledPayoutConfig path.
findKernelSweepRule ::
  (BeamFlow m r) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  m (Maybe STR.ScheduledTransferRule)
findKernelSweepRule merchantId merchantOpCityId = do
  accCfg <-
    FinanceAccountConfig.getAccountConfig
      FinanceAccountConfig.AccountClass
        { merchantId = merchantId.getId,
          merchantOperatingCityId = merchantOpCityId.getId,
          accountType = FinanceAccount.Liability,
          counterpartyType = Just FinanceAccount.DRIVER,
          subLedger = Nothing
        }
  case accCfg.autoTransferOut of
    Nothing -> pure Nothing
    Just ruleName -> do
      mbRule <- QSTR.findByName merchantOpCityId.getId ruleName
      pure $ mbRule >>= \rule -> if rule.isEnabled then Just rule else Nothing

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
    DSPC.HOURLY ->
      -- Rolling from now; timeOfDay is ignored. intervalHours is validated at upsert.
      pure $ Time.addUTCTime (fromIntegral (fromMaybe 1 config.intervalHours) * 3600) now
    DSPC.EVERY_N_DAYS -> do
      -- Anchored to timeOfDay so it does not drift. intervalDays is validated at upsert.
      let n = max 1 (fromMaybe 1 config.intervalDays)
          nextLocalDay = Time.addDays (fromIntegral n) localDay
      pure $ Time.addUTCTime (negate timeDiff) (Time.UTCTime nextLocalDay targetTimeOfDay)

-- | Parse "HH:MM" into (hours, minutes). Defaults to (2, 0) on failure.
parseTimeOfDay :: Text -> (Int, Int)
parseTimeOfDay t =
  case break (== ':') (toString t) of
    (hh, ':' : mm) -> case (readMaybe hh, readMaybe mm) of
      (Just h, Just m) -> (h, m)
      _ -> (2, 0)
    _ -> (2, 0)
