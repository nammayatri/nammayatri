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
    claimBulkPayoutSlot,
    submitBulkBatch,
    submitOneChunk,
    getOrCreatePayoutRun,
    createAdhocPayoutRun,
    reconcileBulkBatches,
    scheduleBulkPollJob,
    failClaimedOrder,
    parsePayoutRail,
    payoutPartnerLabel,
    beneficiaryTypeFromRole,
  )
where

import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Time as Time
import qualified Data.Time.Calendar.WeekDate as Time
import Domain.Action.UI.DriverWallet
  ( PayoutContext (..),
    initiateWalletPayout,
  )
import Domain.Action.UI.Ride.EndRide.Internal (makeWalletRunningBalanceLockKey)
import qualified Domain.Types.DriverBankAccount
import qualified Domain.Types.DriverInformation as DI
import Domain.Types.Extra.Plan
import qualified Domain.Types.FleetOwnerInformation as DFOI
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantServiceConfig as DEMSC
import qualified Domain.Types.PayoutRun as DDPR
import qualified Domain.Types.Person as DP
import qualified Domain.Types.ScheduledPayoutConfig as DSPC
import qualified Domain.Types.TransporterConfig as DTConf
import qualified Kernel.External.Notification.FCM.Types as FCM
import qualified Kernel.External.Payout.Interface as Payout
import Kernel.External.Types (SchedulerFlow, ServiceFlow)
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import Kernel.Types.Documents (VerificationStatus (..))
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified Lib.Finance.Core.Types as Finance
import qualified Lib.Finance.Ledger.Service as LedgerService
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Payment.Domain.Types.Common as DPayment
import qualified Lib.Payment.Domain.Types.PayoutBatch as DPayoutBatch
import qualified Lib.Payment.Domain.Types.PayoutBatchExclusion as DPayoutBatchExclusion
import qualified Lib.Payment.Domain.Types.PayoutOrder as DPayoutOrder
import qualified Lib.Payment.Domain.Types.PayoutRequest as PR
import Lib.Payment.Payout.Request (updateStatusWithHistoryById)
import qualified Lib.Payment.Storage.Beam.BeamFlow as PaymentBeamFlow
import qualified Lib.Payment.Storage.Queries.PayoutBatch as QPayoutBatch
import qualified Lib.Payment.Storage.Queries.PayoutBatchExclusion as QPayoutBatchExclusion
import qualified Lib.Payment.Storage.Queries.PayoutOrder as QPayoutOrder
import qualified Lib.Payment.Storage.Queries.PayoutRequest as QPR
import Lib.Scheduler
import Lib.Scheduler.JobStorageType.SchedulerType (createJobInWithCheck)
import SharedLogic.Allocator
import SharedLogic.Finance.Wallet
import Storage.Beam.Payment ()
import Storage.Beam.SchedulerJob ()
import qualified Storage.CachedQueries.Merchant as CQM
import Storage.ConfigPilot.Config.ScheduledPayoutConfig (ScheduledPayoutConfigDimensions (..))
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.DriverBankAccount as QDBA
import qualified Storage.Queries.DriverInformationExtra as QDIE
import qualified Storage.Queries.FleetOwnerInformationExtra as QFOIE
import qualified Storage.Queries.PayoutRun as QPayoutRun
import qualified Storage.Queries.Person as QPerson
import qualified Tools.Notifications as Notify
import qualified Tools.Payout as TPayout

--------------------------------------------------------------------------------
-- Job entry point
--------------------------------------------------------------------------------

sendScheduledBatchPayout ::
  ( EncFlow m r,
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
        else processCategory id.getId config jobData

--------------------------------------------------------------------------------
-- Category dispatch
--------------------------------------------------------------------------------

processCategory ::
  ( EncFlow m r,
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
  Text -> -- stable scheduler job id for this run (Handler.hs mutex key); identifies the payout_run
  DSPC.ScheduledPayoutConfig ->
  ScheduledBatchPayoutJobData ->
  m ExecutionResult
processCategory jobId config jobData = do
  case config.payoutCategory of
    DPayment.DRIVER_WALLET_TRANSACTION -> processWalletPayouts jobId config jobData
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
  Text ->
  DSPC.ScheduledPayoutConfig ->
  ScheduledBatchPayoutJobData ->
  m ExecutionResult
processWalletPayouts jobId config jobData = do
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
      let driverCursorKey = "ScheduledBatchPayout:Cursor:" <> merchantOpCityId.getId <> ":" <> show config.payoutCategory
          fleetCursorKey = "ScheduledBatchPayout:Cursor:Fleet:" <> merchantOpCityId.getId <> ":" <> show config.payoutCategory

      -- Process drivers
      mbLastDriverId <- Redis.get driverCursorKey
      (payoutServiceFlow, payoutServiceName) <- TPayout.getPayoutServiceFlowForMerchant (.createPayoutOrder) (TPayout.SubscriptionConfigOption PREPAID_SUBSCRIPTION) DEMSC.PayoutService merchantOpCityId
      let isPayoutVpaRequired = case payoutServiceFlow of
            Payout.JuspayFlow -> True
            Payout.StripeFlow -> False
            Payout.BulkFlow -> False -- HDFC CBX is account-and-IFSC based, not VPA based
      run <- getOrCreatePayoutRun jobId merchantId merchantOpCityId transporterConfig.currency payoutServiceFlow
      eligibleDriverInfos <- QDIE.findEligibleForScheduledPayout merchantOpCityId config.batchSize mbLastDriverId isPayoutVpaRequired
      unless (null eligibleDriverInfos) $ do
        let lastDriverId = (.driverId) $ last eligibleDriverInfos
        Redis.setExp driverCursorKey lastDriverId 86400

      -- Process fleet owners
      mbLastFleetId <- Redis.get fleetCursorKey
      eligibleFleetInfos <- QFOIE.findEligibleFleetOwnersForScheduledPayout merchantOpCityId config.batchSize mbLastFleetId isPayoutVpaRequired
      logDebug $
        "[SBP-DEBUG] mocId=" <> merchantOpCityId.getId
          <> " isPayoutVpaRequired="
          <> show isPayoutVpaRequired
          <> " minimumPayoutAmount="
          <> show config.minimumPayoutAmount
          <> " batchSize="
          <> show config.batchSize
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
        Redis.setExp fleetCursorKey lastFleetId 86400

      let tickCount = length eligibleDriverInfos + length eligibleFleetInfos
      case payoutServiceFlow of
        Payout.BulkFlow -> do
          -- No live per-driver API call happens here (HDFC CBX has none) -- claiming a slot is
          -- just DB writes, so this runs synchronously rather than via 'fork', because the
          -- claimed orders need to be collected into one bulk submission below.
          driverOrders <-
            catMaybes
              <$> forM eligibleDriverInfos (\driverInfo -> claimBulkPayoutSlot config PR.SCHEDULED transporterConfig merchantId merchantOpCityId run.id.getId driverInfo.driverId (driverInfo.payoutVpaStatus == Just DI.MANUALLY_ADDED))
          fleetOrders <-
            catMaybes
              <$> forM eligibleFleetInfos (\fleetInfo -> claimBulkPayoutSlot config PR.SCHEDULED transporterConfig merchantId merchantOpCityId run.id.getId fleetInfo.fleetOwnerPersonId (fleetInfo.payoutVpaStatus == Just DFOI.MANUALLY_ADDED))
          let claimedOrders = driverOrders <> fleetOrders
          excludedAtSubmit <-
            if null claimedOrders
              then pure []
              else submitBulkBatch config payoutServiceName merchantId merchantOpCityId run.id.getId transporterConfig.currency DPayoutBatch.SCHEDULED claimedOrders
          -- Recover SUBMIT_UNKNOWN batches and poll due ones, regardless of which tick submitted them.
          reconcileBulkBatches payoutServiceName merchantId merchantOpCityId run.id.getId (parsePayoutRail config.defaultPayoutRail)
          -- Every exclusion (claim-time NOT_REGISTERED_WITH_PARTNER, and submit-time
          -- BANK_DETAILS_MISSING/UNVERIFIED) already writes a payout_batch_exclusion row via
          -- recordExclusion; mirror the same tally into the run's own counters so the dashboard
          -- doesn't read includedCount == evaluatedCount / excludedCount == 0 regardless of what
          -- actually happened.
          let claimTimeExcluded = tickCount - length claimedOrders
              submitTimeExcluded = length excludedAtSubmit
          QPayoutRun.updateAssemblyCounts
            (run.evaluatedCount + tickCount)
            (run.excludedCount + claimTimeExcluded + submitTimeExcluded)
            (run.includedCount + length claimedOrders - submitTimeExcluded)
            run.id
        _ -> do
          for_ eligibleDriverInfos $ \driverInfo ->
            fork ("ScheduledWalletPayout:Driver:" <> driverInfo.driverId.getId) $
              processOneWalletPayout
                config
                PR.SCHEDULED
                transporterConfig
                merchantId
                merchantOpCityId
                driverInfo.driverId
                driverInfo.payoutVpa
                (driverInfo.payoutVpaStatus == Just DI.MANUALLY_ADDED)
          for_ eligibleFleetInfos $ \fleetInfo ->
            fork ("ScheduledWalletPayout:Fleet:" <> fleetInfo.fleetOwnerPersonId.getId) $
              processOneWalletPayout
                config
                PR.SCHEDULED
                transporterConfig
                merchantId
                merchantOpCityId
                fleetInfo.fleetOwnerPersonId
                fleetInfo.payoutVpa
                (fleetInfo.payoutVpaStatus == Just DFOI.MANUALLY_ADDED)
          -- No exclusion-table concept exists for Juspay/Stripe -- findEligibleForScheduledPayout
          -- already pre-filters to eligible beneficiaries, and each is attempted, so every
          -- evaluated beneficiary here is also "included".
          unless (tickCount == 0) $
            QPayoutRun.updateAssemblyCounts (run.evaluatedCount + tickCount) run.excludedCount (run.includedCount + tickCount) run.id

      if null eligibleDriverInfos && null eligibleFleetInfos
        then do
          Redis.del driverCursorKey
          Redis.del fleetCursorKey
          now <- getCurrentTime
          -- Bulk batches resolve asynchronously (HDFC poll-only) -- if any are still in flight,
          -- seal SEALED (not COMPLETED) so a dashboard viewer never sees a "done" run with stale
          -- zero counts; BulkBatchPayoutPoll.sealRun owns the final COMPLETED/PARTIALLY_RESOLVED
          -- transition once every batch is terminal. Non-bulk (Juspay/Stripe) has no run-level
          -- batch tracking -- each payout resolves independently via webhook -- so COMPLETED here
          -- is correct for that flow.
          if payoutServiceFlow == Payout.BulkFlow
            then do
              batches <- QPayoutBatch.findAllByRunId (Just run.id.getId)
              let hasUnresolved = any (\b -> b.status `notElem` [DPayoutBatch.COMPLETED, DPayoutBatch.REJECTED]) batches
                  runTotalAmount = if null batches then Nothing else Just (sum (map (.totalAmount) batches))
              if hasUnresolved
                then do
                  QPayoutRun.sealRun DDPR.SEALED runTotalAmount (length batches) (Just now) run.id
                  scheduleBulkPollJob run merchantId merchantOpCityId payoutServiceName config
                else QPayoutRun.sealRun DDPR.COMPLETED runTotalAmount (length batches) (Just now) run.id
            else QPayoutRun.sealRun DDPR.COMPLETED Nothing 0 (Just now) run.id
          nextTime <- computeNextRunTime config
          logInfo $ "All drivers and fleet owners processed. Next run at: " <> show nextTime
          pure $ ReSchedule nextTime
        else do
          nextBatch <- addUTCTime 5 <$> getCurrentTime
          pure $ ReSchedule nextBatch

-- | Fetch the payout_run row for this scheduler job (stable across every 'ReSchedule' tick of the
--   same run), creating it on the run's first tick.
getOrCreatePayoutRun ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Text -> -- parentJobId
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Currency ->
  Payout.PayoutServiceFlow ->
  m DDPR.PayoutRun
getOrCreatePayoutRun jobId merchantId merchantOpCityId currency payoutServiceFlow = do
  now <- getCurrentTime
  let today = Time.utctDay now
  -- Keyed on (parentJobId, valueDate): the job's own id is stable forever (self-rescheduling), so
  -- parentJobId alone would keep reusing the very first run ever created instead of one per day.
  mbRun <- QPayoutRun.findByParentJobIdAndValueDate (Just jobId) today
  case mbRun of
    Just run -> pure run
    Nothing -> do
      runId <- generateGUID
      let run =
            DDPR.PayoutRun
              { id = Id runId,
                merchantId = merchantId,
                merchantOperatingCityId = merchantOpCityId,
                payoutPartner = payoutPartnerLabel payoutServiceFlow,
                origin = DDPR.SCHEDULED,
                parentJobId = Just jobId,
                status = DDPR.ASSEMBLING,
                scheduledFor = now,
                valueDate = today,
                currency = currency,
                totalAmount = Nothing,
                evaluatedCount = 0,
                excludedCount = 0,
                includedCount = 0,
                batchCount = 0,
                paidCount = 0,
                failedCount = 0,
                pendingCount = 0,
                paidAmount = 0,
                failedAmount = 0,
                debitedAmount = 0,
                sealedAt = Nothing,
                resolvedAt = Nothing,
                createdAt = now,
                updatedAt = now
              }
      QPayoutRun.create run
      pure run

-- | Every adhoc initiate gets its own fresh run -- unlike getOrCreatePayoutRun, there's no
--   scheduler jobId to dedupe on and no "one run per day" semantics.
createAdhocPayoutRun ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Currency ->
  Payout.PayoutServiceFlow ->
  m DDPR.PayoutRun
createAdhocPayoutRun merchantId merchantOpCityId currency payoutServiceFlow = do
  now <- getCurrentTime
  runId <- generateGUID
  let run =
        DDPR.PayoutRun
          { id = Id runId,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOpCityId,
            payoutPartner = payoutPartnerLabel payoutServiceFlow,
            origin = DDPR.ADHOC,
            parentJobId = Nothing,
            status = DDPR.ASSEMBLING,
            scheduledFor = now,
            valueDate = Time.utctDay now,
            currency = currency,
            totalAmount = Nothing,
            evaluatedCount = 0,
            excludedCount = 0,
            includedCount = 0,
            batchCount = 0,
            paidCount = 0,
            failedCount = 0,
            pendingCount = 0,
            paidAmount = 0,
            failedAmount = 0,
            debitedAmount = 0,
            sealedAt = Nothing,
            resolvedAt = Nothing,
            createdAt = now,
            updatedAt = now
          }
  QPayoutRun.create run
  pure run

-- | Spin up the shared HDFC-bulk poll job (BulkBatchPayoutPoll) for a run, so it keeps getting
--   reconciled on its own cadence independently of when the caller's own job next ticks. Needed
--   by both the adhoc flow (which has no other job driving it at all) and the scheduled sweep
--   (whose own job reschedules to the *next* run -- possibly a day/week/month away -- as soon as
--   this run finishes assembling, long before HDFC actually resolves it).
scheduleBulkPollJob ::
  (JobCreator r m, Redis.HedisLTSFlowEnv r) =>
  DDPR.PayoutRun ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  DEMSC.ServiceName ->
  DSPC.ScheduledPayoutConfig ->
  m ()
scheduleBulkPollJob run merchantId merchantOpCityId payoutServiceName config = do
  now <- getCurrentTime
  -- Deadline is derived from the run's own (fixed) createdAt, not from 'now' at call time --
  -- createJobInWithCheck's dedup is exact-JSON-equality on the whole jobData, so if this were
  -- `addUTCTime 86400 now`, a retried call would compute a slightly different timestamp and the
  -- dedup check would silently fail to recognize it as the same job.
  let deadline = addUTCTime 86400 run.createdAt
  Redis.runInMasterCloudRedisCell $
    -- createJobInWithCheck (not a bare createJobIn) so a retried tick that already succeeded in
    -- creating this run's poll job once doesn't create a second one for it.
    createJobInWithCheck @_ @'BulkBatchPayoutPoll
      (Just merchantId)
      (Just merchantOpCityId)
      (16 * 60) -- first tick just after the +15min inquiry window opens
      (addUTCTime (-3600) now)
      (addUTCTime 3600 deadline)
      "BulkBatchPayoutPoll"
      (Just 1)
      BulkBatchPayoutPollJobData
        { runId = run.id,
          merchantId = merchantId,
          merchantOperatingCityId = merchantOpCityId,
          payoutServiceName = payoutServiceName,
          payoutRail = parsePayoutRail config.defaultPayoutRail,
          deadline = deadline
        }

payoutPartnerLabel :: Payout.PayoutServiceFlow -> Text
payoutPartnerLabel = \case
  Payout.JuspayFlow -> "JUSPAY"
  Payout.StripeFlow -> "STRIPE"
  Payout.BulkFlow -> "HDFC_CBX"

--------------------------------------------------------------------------------
-- HDFC CBX / BulkFlow: batch assembly, submission and polling
--------------------------------------------------------------------------------

-- | Claim a payout_order slot for one beneficiary without forking, so the caller can collect and
--   submit every order claimed this tick together (mirrors processOneWalletPayout's eligibility check).
claimBulkPayoutSlot ::
  ( EncFlow m r,
    CacheFlow m r,
    Finance.HasActorInfo m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    BeamFlow m r,
    PaymentBeamFlow.BeamFlow m r,
    ServiceFlow m r,
    HasFlowEnv m r '["selfBaseUrl" ::: BaseUrl],
    Redis.HedisLTSFlowEnv r
  ) =>
  DSPC.ScheduledPayoutConfig ->
  PR.PayoutType -> -- SCHEDULED or ADHOC
  DTConf.TransporterConfig ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Text -> -- payout_run id
  Id DP.Person ->
  Bool -> -- isManuallyAdded
  m (Maybe DPayoutOrder.PayoutOrder)
claimBulkPayoutSlot config payoutType transporterConfig merchantId merchantOpCityId runId personId isManuallyAdded = do
  resultRef <- liftIO $ newIORef Nothing
  result <- try $ do
    person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
    let counterparty = counterpartyFromRole person.role
        beneficiaryType = beneficiaryTypeFromRole person.role
        ctx =
          PayoutContext
            { driverId = personId,
              merchantId = merchantId,
              mocId = merchantOpCityId,
              person = person,
              payoutVpa = Nothing,
              transporterConfig = transporterConfig
            }
    Redis.withWaitOnLockRedisWithExpiry (makeWalletRunningBalanceLockKey personId.getId) 10 10 $ do
      now <- getCurrentTime
      mbAccount <- getWalletAccountByOwner counterparty personId.getId
      let mbAccountId = (.id) <$> mbAccount
      walletBalance <- fromMaybe 0 <$> getWalletBalanceByOwner counterparty personId.getId
      let timeDiff = secondsToNominalDiffTime transporterConfig.timeDiffFromUtc
          cutOffDays = transporterConfig.driverWalletConfig.payoutCutOffDays
          cutoff = payoutCutoffTimeUTC timeDiff cutOffDays now
      (nonRedeemable, redeemableIds, merchantTransferAmt) <- case mbAccountId of
        Nothing -> pure (0, [], 0)
        Just accountId -> getPayoutEligibilityData accountId cutoff now
      let payoutableBalance = walletBalance - nonRedeemable
      mbOrder <-
        if isManuallyAdded
          then do
            recordExclusion merchantId runId personId beneficiaryType DPayoutBatchExclusion.NOT_REGISTERED_WITH_PARTNER (Just payoutableBalance)
            pure Nothing
          else
            if payoutableBalance >= config.minimumPayoutAmount
              then initiateWalletPayout ctx payoutableBalance payoutType Nothing (Just cutoff) (map (.getId) redeemableIds) merchantTransferAmt
              else do
                -- Common case on every sweep and not actionable by anyone -- log only, don't persist.
                logDebug $ "ScheduledBulkPayoutClaim: skipping " <> personId.getId <> ", payoutableBalance=" <> show payoutableBalance <> " below minimum=" <> show config.minimumPayoutAmount
                pure Nothing
      liftIO $ writeIORef resultRef mbOrder
  case result of
    Left (e :: SomeException) -> do
      logError $ "ScheduledBulkPayoutClaim error for " <> personId.getId <> ": " <> show e
      pure Nothing
    Right () -> liftIO $ readIORef resultRef

beneficiaryTypeFromRole :: DP.Role -> DPayoutBatchExclusion.PayoutBatchExclusionBeneficiaryType
beneficiaryTypeFromRole role
  | role `elem` [DP.FLEET_OWNER, DP.FLEET_BUSINESS] = DPayoutBatchExclusion.FLEET_OWNER
  | otherwise = DPayoutBatchExclusion.DRIVER

-- | Record why one beneficiary was skipped from this run's bulk submission, so ops can audit a
--   run without having to reconstruct eligibility after the fact.
recordExclusion ::
  (MonadFlow m, PaymentBeamFlow.BeamFlow m r) =>
  Id DM.Merchant ->
  Text -> -- payout_run id
  Id DP.Person ->
  DPayoutBatchExclusion.PayoutBatchExclusionBeneficiaryType ->
  DPayoutBatchExclusion.PayoutBatchExclusionReason ->
  Maybe HighPrecMoney ->
  m ()
recordExclusion merchantId runId personId beneficiaryType reason mbBalance = do
  now <- getCurrentTime
  exclusionId <- generateGUID
  QPayoutBatchExclusion.create
    DPayoutBatchExclusion.PayoutBatchExclusion
      { id = Id exclusionId,
        merchantId = merchantId.getId,
        runId = runId,
        beneficiaryId = personId.getId,
        beneficiaryType = beneficiaryType,
        reason = reason,
        balanceAtEvaluation = mbBalance,
        notifiedAt = Nothing,
        correctedAt = Nothing,
        createdAt = now,
        updatedAt = now
      }

-- | Turn a claimed order into a bulk-payout line item using an already-fetched bank-account map.
--   An order is excluded when there's no fetchable bank account at all, or when one exists but
--   isn't currently VALID (unverified, or previously marked INVALID by a prior HDFC rejection --
--   see failClaimedOrder) -- either way, logged via recordExclusion for audit and released via
--   failClaimedOrder right here so its ledger reservation doesn't stay stuck PROCESSING forever
--   (excluded orders never make it into a batch, so no poll job would ever see them either).
--   Returns Left with the real reason on exclusion, Right with the item to submit otherwise.
buildBulkItem ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r, Finance.HasActorInfo m r, BeamFlow m r, PaymentBeamFlow.BeamFlow m r, Redis.HedisLTSFlowEnv r) =>
  Id DM.Merchant ->
  Text -> -- payout_run id
  Map Text Domain.Types.DriverBankAccount.DriverBankAccount ->
  DPayoutOrder.PayoutOrder ->
  m (Either (DPayoutOrder.PayoutOrder, Text) (DPayoutOrder.PayoutOrder, Payout.BulkPayoutItem))
buildBulkItem merchantId runId bankAccountsByOwner po =
  case Map.lookup po.customerId bankAccountsByOwner of
    Nothing -> exclude DPayoutBatchExclusion.BANK_DETAILS_MISSING "No bank account on file for this beneficiary"
    Just bankAccount
      | bankAccount.verificationStatus /= Just VALID ->
        exclude DPayoutBatchExclusion.BANK_DETAILS_UNVERIFIED ("Bank account is not verified (status: " <> maybe "never verified" show bankAccount.verificationStatus <> ")")
      | otherwise ->
        pure $
          Right
            ( po,
              Payout.BulkPayoutItem
                { itemRef = po.orderId,
                  amount = po.amount.amount,
                  currency = po.amount.currency,
                  bankAccountNumber = bankAccount.accountId,
                  bankIfscCode = fromMaybe "" bankAccount.ifscCode,
                  beneficiaryName = fromMaybe "" bankAccount.nameAtBank,
                  beneficiaryCode = Nothing,
                  beneficiaryEmail = Nothing
                }
            )
  where
    exclude exclusionReason detail = do
      person <- QPerson.findById (Id po.customerId)
      let beneficiaryType = maybe DPayoutBatchExclusion.DRIVER (beneficiaryTypeFromRole . (.role)) person
      recordExclusion merchantId runId (Id po.customerId) beneficiaryType exclusionReason (Just po.amount.amount)
      failClaimedOrder po Payout.INVALID_ACCOUNT detail
      pure $ Left (po, detail)

-- | Group every order claimed this tick into HDFC CBX bulk submission(s). Bank accounts are
--   fetched once in bulk (not per order) to avoid an N+1 query per tick. Returns the orders that
--   got excluded (already released/failed by buildBulkItem), paired with the real reason, so a
--   caller that reports per-person results (the adhoc flow) can tell them apart from the ones
--   actually submitted and surface why.
submitBulkBatch ::
  ( ServiceFlow m r,
    EsqDBFlow m r,
    CacheFlow m r,
    Finance.HasActorInfo m r,
    PaymentBeamFlow.BeamFlow m r,
    Redis.HedisLTSFlowEnv r
  ) =>
  DSPC.ScheduledPayoutConfig ->
  DEMSC.ServiceName ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Text -> -- payout_run id
  Currency ->
  DPayoutBatch.PayoutBatchOrigin ->
  [DPayoutOrder.PayoutOrder] ->
  m [(DPayoutOrder.PayoutOrder, Text)] -- excluded orders with their reason
submitBulkBatch config payoutServiceName merchantId merchantOpCityId runId _currency origin claimedOrders = do
  bankAccounts <- QDBA.getDriverBankAccounts (map (Id . (.customerId)) claimedOrders)
  let bankAccountsByOwner = Map.fromList [(account.driverId.getId, account) | account <- bankAccounts]
  results <- forM claimedOrders (buildBulkItem merchantId runId bankAccountsByOwner)
  let itemPairs = [pair | Right pair <- results]
      excludedOrders = [excluded | Left excluded <- results]
  unless (null itemPairs) $ do
    vsc <- TPayout.getPayoutServiceConfig payoutServiceName merchantOpCityId
    let -- HDFC's cap is authoritative; config.itemsPerBatch can only shrink it, never exceed it.
        partnerCap = case vsc of
          Payout.HdfcCbxConfig cfg -> cfg.maxItemsPerBatch
          _ -> 100
        chunkSize = max 1 (maybe partnerCap (min partnerCap) config.itemsPerBatch)
        rail = parsePayoutRail config.defaultPayoutRail
    for_ (chunksOf chunkSize itemPairs) $ \chunk ->
      submitOneChunk vsc merchantId merchantOpCityId runId origin rail Nothing chunk
  pure excludedOrders

-- | Split a batch-submission slice by HDFC's per-call item cap so each chunk becomes its own
--   payout_batch/submitBulkPayout call, instead of one oversized call HDFC would just refuse.
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- | HDFC's inquiry budget (Inquiry Frequency Interval.docx, verbatim): "Daily maximum of 6
--   attempts are allowed for a particular batch number for re-inquiry." Same for every rail --
--   the budget is per batch number, not per payout rail. Cadence: first inquiry at
--   submission+15min, then 5-minute gaps until the day's 6 attempts are spent, then one check
--   per day (roughly EOD) until the 3-day inquiry window closes.
firstInquiryDelay :: NominalDiffTime
firstInquiryDelay = 15 * 60

interInquiryGap :: NominalDiffTime
interInquiryGap = 5 * 60

maxAttemptsPerDay :: Int
maxAttemptsPerDay = 6

-- | HDFC's inquiry window closes three days after submission -- a batch still unresolved past
--   this stops being polled and needs manual reconciliation (visible on the payout-batch dashboard).
inquiryWindow :: NominalDiffTime
inquiryWindow = 3 * 86400

-- | Mark a batch SUBMITTED (with whatever partner ref it got, if any) and schedule its first
--   inquiry. Shared by a fresh submission, a recovered SUBMIT_UNKNOWN batch, and a resubmitted one.
markSubmittedAndScheduleInquiry ::
  (MonadFlow m, PaymentBeamFlow.BeamFlow m r) =>
  Id DPayoutBatch.PayoutBatch ->
  Maybe Text ->
  m ()
markSubmittedAndScheduleInquiry batchId mbPartnerRef = do
  submittedAt <- Just <$> getCurrentTime
  QPayoutBatch.markSubmitted DPayoutBatch.SUBMITTED mbPartnerRef submittedAt batchId
  nextInquiryAt <- Just . addUTCTime firstInquiryDelay <$> getCurrentTime
  today <- Just . Time.utctDay <$> getCurrentTime
  QPayoutBatch.updateInquiryState 0 today nextInquiryAt batchId

-- | Submit one chunk as its own payout_batch. 'retryOf' is Nothing for a fresh submission, or the
--   previous batch id when resubmitting the same items -- a batch retries at most once; a second
--   BulkRejected fails the items for real instead of looping.
submitOneChunk ::
  (ServiceFlow m r, EsqDBFlow m r, CacheFlow m r, Finance.HasActorInfo m r, PaymentBeamFlow.BeamFlow m r, Redis.HedisLTSFlowEnv r) =>
  Payout.PayoutServiceConfig ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Text -> -- payout_run id
  DPayoutBatch.PayoutBatchOrigin ->
  Payout.PayoutRail ->
  Maybe (Id DPayoutBatch.PayoutBatch) -> -- retryOfBatchId
  [(DPayoutOrder.PayoutOrder, Payout.BulkPayoutItem)] ->
  m ()
submitOneChunk vsc merchantId merchantOpCityId runId origin rail retryOf itemPairs = do
  now <- getCurrentTime
  let (ordersWithItems, bulkItems) = unzip itemPairs
      valueDate = Time.utctDay now
  batchIdRaw <- generateGUID
  let batchId = Id batchIdRaw
  clientRefNo <- (.getShortId) <$> generateShortId
  let batch =
        DPayoutBatch.PayoutBatch
          { id = batchId,
            merchantId = merchantId.getId,
            merchantOperatingCityId = Just merchantOpCityId.getId,
            runId = Just runId,
            origin = origin,
            status = DPayoutBatch.OPEN,
            payoutRail = payoutRailLabel rail,
            valueDate = valueDate,
            clientRefNo = clientRefNo,
            partnerBatchRef = Nothing,
            itemCount = length bulkItems,
            totalAmount = sum (map (.amount) bulkItems),
            processedCount = 0,
            rejectedCount = 0,
            pendingCount = length bulkItems,
            inquiryAttemptsToday = 0,
            inquiryQuotaDate = Nothing,
            nextInquiryAt = Nothing,
            retryOfBatchId = retryOf,
            partnerResponseCode = Nothing,
            failureReason = Nothing,
            submittedAt = Nothing,
            resolvedAt = Nothing,
            createdAt = now,
            updatedAt = now
          }
  QPayoutBatch.create batch
  for_ ordersWithItems $ \po -> QPayoutOrder.updateBatchId (Just batchId) po.orderId

  let req = Payout.BulkPayoutReq {clientRefNo = clientRefNo, valueDate = valueDate, rail = rail, items = bulkItems}
  submitResult <- try $ Payout.submitBulkPayout vsc req
  case submitResult of
    Left (e :: SomeException) -> do
      logError $ "HDFC CBX bulk submit errored/timed out for batch " <> batchIdRaw <> ": " <> show e
      -- Reservations stay held: the call may have landed even though we didn't hear back.
      -- Recovered on a later tick via reconcileBulkBatches, not here.
      QPayoutBatch.updateStatus DPayoutBatch.SUBMIT_UNKNOWN batchId
    Right (Payout.BulkAccepted partnerBatchRef) -> markSubmittedAndScheduleInquiry batchId (Just partnerBatchRef)
    Right (Payout.BulkDuplicate (Just partnerBatchRef)) -> markSubmittedAndScheduleInquiry batchId (Just partnerBatchRef)
    Right (Payout.BulkDuplicate Nothing) -> do
      -- HDFC acked a duplicate but gave no batchnum -- inquireBulkPayout hard-requires
      -- partnerBatchRef and would just throw on every poll if we left it SUBMITTED with
      -- Nothing. Mark SUBMIT_UNKNOWN first (so a later tick retries via reconcileBulkBatches
      -- even if this immediate attempt itself errors out) and recover the ref right now via
      -- clientRefNo instead of waiting a full cycle.
      logInfo $ "HDFC CBX bulk duplicate ack for batch " <> batchIdRaw <> " came without a batchnum; recovering it immediately via clientRefNo"
      QPayoutBatch.updateStatus DPayoutBatch.SUBMIT_UNKNOWN batchId
      recoverOneBatch vsc merchantId merchantOpCityId runId rail batch
    Right (Payout.BulkRejected code reason) -> do
      logError $ "HDFC CBX bulk batch " <> batchIdRaw <> " rejected: " <> code <> " " <> reason
      QPayoutBatch.updateFailure DPayoutBatch.REJECTED (Just code) (Just reason) batchId
      case retryOf of
        Nothing -> do
          logInfo $ "Retrying batch " <> batchIdRaw <> " once with a fresh clientRefNo after BulkRejected"
          submitOneChunk vsc merchantId merchantOpCityId runId origin rail (Just batchId) itemPairs
        Just _ -> for_ ordersWithItems $ \po -> failClaimedOrder po Payout.REJECTED_AT_VALIDATION reason

-- | Mark a claimed order as failed and, unless the money actually moved (RETURNED_AFTER_DEBIT,
--   which needs manual reconciliation instead), release its ledger reservation. When the reason
--   is specifically about the bank account itself (INVALID_ACCOUNT/ACCOUNT_BLOCKED -- whether from
--   HDFC's inquiry response, or from buildBulkItem finding the account unverified before ever
--   submitting), also flips DriverBankAccount.verificationStatus to INVALID so the next sweep
--   skips this beneficiary instead of reserving-then-releasing the same doomed claim every time.
--   Cleared back to VALID the same way it already is today: BankAccountVerification.getInfoBankAccount
--   sets it on a successful re-verification, so a driver who fixes their account becomes payable
--   again automatically, no separate reset path needed here.
failClaimedOrder ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r, Finance.HasActorInfo m r, BeamFlow m r, PaymentBeamFlow.BeamFlow m r, Redis.HedisLTSFlowEnv r) =>
  DPayoutOrder.PayoutOrder ->
  Payout.BulkFailureReason ->
  Text ->
  m ()
failClaimedOrder po reason detail = do
  QPayoutOrder.updatePayoutOrderStatusAndTransferStatus Payout.FAILURE (Just Payout.TRANSFER_FAILED) po.orderId
  QPayoutOrder.updateFailureCategory (Just (show reason)) po.orderId
  whenJust (listToMaybe =<< po.entityIds) $ \prId -> do
    mbPr <- QPR.findById (Id prId)
    whenJust mbPr $ \pr -> do
      unless (reason == Payout.RETURNED_AFTER_DEBIT) $
        whenJust pr.ledgerEntryIds $ \entryIds ->
          unless (null entryIds) $ LedgerService.markEntriesAsUnsettled (map Id entryIds)
      updateStatusWithHistoryById PR.AUTO_PAY_FAILED (Just detail) pr
  when (reason `elem` [Payout.INVALID_ACCOUNT, Payout.ACCOUNT_BLOCKED]) $ do
    now <- getCurrentTime
    QDBA.updateVerificationStatus (Just INVALID) (Just now) (Id po.customerId)
  -- Forked: this runs inside pollOneBatch's sequential per-item outcome loop, so a blocking FCM
  fork ("BulkPayoutNotify:" <> po.orderId) $ notifyBulkPayoutOutcome po (Just reason)

-- | Settle a claimed order HDFC CBX reports as processed: mark it paid and release the ledger
--   entries into PAID_OUT.
settleClaimedOrder ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r, Finance.HasActorInfo m r, BeamFlow m r, PaymentBeamFlow.BeamFlow m r, Redis.HedisLTSFlowEnv r) =>
  DPayoutOrder.PayoutOrder ->
  Text -> -- settlementRef (UTR / FT number / RRN, per refType)
  Payout.SettlementRefType ->
  m ()
settleClaimedOrder po settlementRef refType = do
  QPayoutOrder.updatePayoutOrderStatusAndTransferStatus Payout.SUCCESS (Just Payout.TRANSFERRED) po.orderId
  QPayoutOrder.updateSettlementRef (Just settlementRef) (Just refType) po.orderId
  whenJust (listToMaybe =<< po.entityIds) $ \prId -> do
    mbPr <- QPR.findById (Id prId)
    whenJust mbPr $ \pr -> do
      -- Mirror the settlement instrument onto payout_request too -- WS14 recon joins bank
      -- response (UTR/FT/RRN) to internal records, and payout_request is the settlement-facing
      -- row (payout_order is NY-internal); keeping both in sync avoids recon needing a fragile
      -- order->request join just to find the UTR.
      QPR.updateSettlementRef (Just settlementRef) (Just refType) pr.id
      whenJust pr.ledgerEntryIds $ \entryIds ->
        unless (null entryIds) $ LedgerService.markEntriesAsPaidOut (map Id entryIds) pr.id.getId
      updateStatusWithHistoryById PR.CREDITED Nothing pr
  -- Forked for the same reason as failClaimedOrder's notify call -- see comment there.
  fork ("BulkPayoutNotify:" <> po.orderId) $ notifyBulkPayoutOutcome po Nothing

-- | Notify the beneficiary of a terminal bulk-payout outcome. On failure, the message names the
--   actual reason HDFC CBX gave instead of a generic "failed".
--   Drivers only for now -- FCM is the only notification channel wired up; fleet owners have no
--   channel decided yet, so they're skipped rather than silently sent to a driver-shaped push.
notifyBulkPayoutOutcome ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r, Redis.HedisLTSFlowEnv r) =>
  DPayoutOrder.PayoutOrder ->
  Maybe Payout.BulkFailureReason -> -- Nothing on success
  m ()
notifyBulkPayoutOutcome po mbFailureReason = do
  mbPerson <- QPerson.findById (Id po.customerId)
  whenJust mbPerson $ \person -> when (beneficiaryTypeFromRole person.role == DPayoutBatchExclusion.DRIVER) $ do
    let amount = po.amount.amount
        (notificationTitle, notificationMessage, notificationType) = case mbFailureReason of
          Nothing -> ("Payout Complete", "Your payout of Rs." <> show amount <> " has been successfully settled to your bank account.", FCM.PAYOUT_COMPLETED)
          Just reason ->
            ( "Payout Failed",
              "Your payout of Rs." <> show amount <> " has failed: " <> bulkFailureReasonMessage reason <> ". Please retry or contact support.",
              FCM.PAYOUT_FAILED
            )
    Notify.sendNotificationToDriver person.merchantOperatingCityId FCM.SHOW Nothing notificationType notificationTitle notificationMessage person person.deviceToken

bulkFailureReasonMessage :: Payout.BulkFailureReason -> Text
bulkFailureReasonMessage = \case
  Payout.INVALID_ACCOUNT -> "your bank account details could not be validated"
  Payout.ACCOUNT_BLOCKED -> "your bank account is currently blocked"
  Payout.RETURNED_AFTER_DEBIT -> "the amount was returned by your bank after being debited"
  Payout.REJECTED_AT_VALIDATION -> "the request was rejected during validation"
  Payout.UNRESOLVED -> "the payout status could not be confirmed in time"

recoverOneBatch ::
  (ServiceFlow m r, EsqDBFlow m r, CacheFlow m r, Finance.HasActorInfo m r, PaymentBeamFlow.BeamFlow m r, Redis.HedisLTSFlowEnv r) =>
  Payout.PayoutServiceConfig ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Text -> -- payout_run id
  Payout.PayoutRail ->
  DPayoutBatch.PayoutBatch ->
  m ()
recoverOneBatch vsc merchantId merchantOpCityId runId rail batch = do
  let req = Payout.BatchRefRecoveryReq {clientRefNo = batch.clientRefNo, valueDate = batch.valueDate}
  result <- try $ Payout.recoverBatchRef vsc req
  case result of
    Left (e :: SomeException) ->
      -- Still unknown; retried again on the next tick.
      logError $ "HDFC CBX batch-ref recovery errored for batch " <> batch.id.getId <> ": " <> show e
    Right (Payout.BatchRefFound partnerBatchRef) ->
      -- It did land: start polling it like any other submitted batch.
      markSubmittedAndScheduleInquiry batch.id (Just partnerBatchRef)
    Right Payout.BatchRefNotFound -> do
      -- Genuinely never landed -- safe to resubmit the same items under a fresh batch/clientRefNo.
      orders <- QPayoutOrder.findAllByBatchId (Just batch.id)
      bankAccounts <- QDBA.getDriverBankAccounts (map (Id . (.customerId)) orders)
      let bankAccountsByOwner = Map.fromList [(account.driverId.getId, account) | account <- bankAccounts]
      results <- forM orders (buildBulkItem merchantId runId bankAccountsByOwner)
      let itemPairs = [pair | Right pair <- results]
      if null itemPairs
        then QPayoutBatch.updateFailure DPayoutBatch.REJECTED Nothing (Just "No items left to resubmit -- all were excluded (see payout_batch_exclusion)") batch.id
        else submitOneChunk vsc merchantId merchantOpCityId runId batch.origin rail (Just batch.id) itemPairs
    Right (Payout.BatchRefRefused code reason) -> do
      logError $ "HDFC CBX batch-ref recovery refused for batch " <> batch.id.getId <> ": " <> code <> " " <> reason
      QPayoutBatch.updateFailure DPayoutBatch.REJECTED (Just code) (Just reason) batch.id
      orders <- QPayoutOrder.findAllByBatchId (Just batch.id)
      for_ orders $ \po -> failClaimedOrder po Payout.REJECTED_AT_VALIDATION reason

-- | Recover SUBMIT_UNKNOWN batches and poll due ones in one pass -- a single findAllByRunId and
--   getPayoutServiceConfig shared across both, instead of two full-run fetches per tick.
reconcileBulkBatches ::
  ( ServiceFlow m r,
    EsqDBFlow m r,
    CacheFlow m r,
    Finance.HasActorInfo m r,
    PaymentBeamFlow.BeamFlow m r,
    Redis.HedisLTSFlowEnv r
  ) =>
  DEMSC.ServiceName ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Text -> -- payout_run id
  Payout.PayoutRail ->
  m ()
reconcileBulkBatches payoutServiceName merchantId merchantOpCityId runId rail = do
  batches <- QPayoutBatch.findAllByRunId (Just runId)
  now <- getCurrentTime
  let today = Time.utctDay now
      unknownBatches = filter ((== DPayoutBatch.SUBMIT_UNKNOWN) . (.status)) batches
      dueBatches =
        filter
          ( \b ->
              b.status `elem` [DPayoutBatch.SUBMITTED, DPayoutBatch.PARTIALLY_RESOLVED, DPayoutBatch.AWAITING_PARTNER_APPROVAL]
                && maybe False (<= now) b.nextInquiryAt
                && attemptsToday today b < dailyAttemptCap today b
          )
          batches
  unless (null unknownBatches && null dueBatches) $ do
    vsc <- TPayout.getPayoutServiceConfig payoutServiceName merchantOpCityId
    for_ unknownBatches $ \batch -> recoverOneBatch vsc merchantId merchantOpCityId runId rail batch
    for_ dueBatches $ \batch -> pollOneBatch vsc batch

pollOneBatch ::
  ( ServiceFlow m r,
    EsqDBFlow m r,
    CacheFlow m r,
    Finance.HasActorInfo m r,
    PaymentBeamFlow.BeamFlow m r,
    Redis.HedisLTSFlowEnv r
  ) =>
  Payout.PayoutServiceConfig ->
  DPayoutBatch.PayoutBatch ->
  m ()
pollOneBatch vsc batch = do
  let req = Payout.BulkInquiryReq {partnerBatchRef = batch.partnerBatchRef, clientRefNo = batch.clientRefNo, valueDate = batch.valueDate}
  result <- try $ Payout.inquireBulkPayout vsc req
  case result of
    Left (e :: SomeException) -> do
      logError $ "HDFC CBX inquiry errored for batch " <> batch.id.getId <> ": " <> show e
      rescheduleInquiry batch
    Right Payout.InquiryNotReady -> rescheduleInquiry batch
    Right Payout.InquiryNoData -> do
      -- Not a retry signal (per the shared-kernel adapter's own doc comment) -- push it far out
      -- rather than looping, and leave it for a manual look.
      now <- getCurrentTime
      let today = Time.utctDay now
          farOut = Just (addUTCTime (86400 * 3) now)
      QPayoutBatch.updateInquiryState (attemptsToday today batch + 1) (Just today) farOut (Id batch.id.getId)
    Right (Payout.InquiryRefused code reason) -> do
      logError $ "HDFC CBX inquiry refused for batch " <> batch.id.getId <> ": " <> code <> " " <> reason
      rescheduleInquiry batch
    Right (Payout.InquiryResolved outcomes) -> do
      results <- forM outcomes $ \(itemRef, outcome) -> applyBulkOutcome itemRef outcome
      let processedCount = length (filter (== ItemOutcomeProcessed) results)
          rejectedCount = length (filter (== ItemOutcomeRejected) results)
          pendingCount = length (filter (== ItemOutcomePending) results)
      if pendingCount > 0
        then do
          QPayoutBatch.updateResolutionCounts DPayoutBatch.PARTIALLY_RESOLVED processedCount rejectedCount pendingCount Nothing (Id batch.id.getId)
          rescheduleInquiry batch
        else do
          resolvedAt <- Just <$> getCurrentTime
          QPayoutBatch.updateResolutionCounts DPayoutBatch.COMPLETED processedCount rejectedCount 0 resolvedAt (Id batch.id.getId)
  where
    -- 5-minute gaps until the day's 6-attempt budget is spent, then one check a day until the
    -- 3-day inquiry window closes, after which polling stops for manual reconciliation.
    rescheduleInquiry b = do
      now <- getCurrentTime
      let today = Time.utctDay now
          attemptsSoFar = attemptsToday today b
          pastInquiryWindow = maybe False (\s -> now >= addUTCTime inquiryWindow s) b.submittedAt
          nextAt
            | pastInquiryWindow = Nothing
            | attemptsSoFar + 1 < dailyAttemptCap today b = Just (addUTCTime interInquiryGap now)
            | otherwise = Just (addUTCTime 86400 now)
      when pastInquiryWindow $
        logWarning $ "Batch " <> b.id.getId <> " passed the 3-day HDFC inquiry window; stopping polling, needs manual reconciliation"
      QPayoutBatch.updateInquiryState (attemptsSoFar + 1) (Just today) nextAt (Id b.id.getId)

-- | inquiryAttemptsToday only counts if inquiryQuotaDate is actually today; a stale count from an
--   earlier day reads as 0.
attemptsToday :: Time.Day -> DPayoutBatch.PayoutBatch -> Int
attemptsToday today b
  | b.inquiryQuotaDate == Just today = b.inquiryAttemptsToday
  | otherwise = 0

-- | HDFC's 6-attempt budget is the submission day's burst; the design doc's cadence is "a set,
--   a second set, then once at EOD for three days" -- i.e. only the submission day gets the full
--   budget, every later day gets exactly one check.
dailyAttemptCap :: Time.Day -> DPayoutBatch.PayoutBatch -> Int
dailyAttemptCap today b
  | (Time.utctDay <$> b.submittedAt) == Just today = maxAttemptsPerDay
  | otherwise = 1

-- | What happened to one item on this inquiry pass, for tallying payout_batch's counts.
data BulkItemResult = ItemOutcomeProcessed | ItemOutcomeRejected | ItemOutcomePending
  deriving (Eq)

-- | Apply one (itemRef, outcome) row from an inquiry to its payout_order.
applyBulkOutcome ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r, Finance.HasActorInfo m r, BeamFlow m r, PaymentBeamFlow.BeamFlow m r, Redis.HedisLTSFlowEnv r) =>
  Text ->
  Payout.BulkItemOutcome ->
  m BulkItemResult
applyBulkOutcome itemRef outcome = do
  mbOrder <- QPayoutOrder.findByOrderId itemRef
  case mbOrder of
    Nothing -> do
      logError $ "HDFC CBX inquiry referenced an unknown order: " <> itemRef
      pure ItemOutcomeRejected
    Just po
      -- Idempotent: HDFC repeats every row on every inquiry, so only act -- and only count as
      -- newly resolved -- if this order is still in flight. An already-terminal order (from a
      -- prior inquiry pass) is reported as its terminal state, not re-applied.
      | po.transferStatus == Just Payout.TRANSFERRED -> pure ItemOutcomeProcessed
      | po.transferStatus == Just Payout.TRANSFER_FAILED -> pure ItemOutcomeRejected
      | otherwise -> case outcome of
        Payout.ItemInterim _ -> pure ItemOutcomePending
        Payout.ItemProcessed settlementRef refType -> settleClaimedOrder po settlementRef refType >> pure ItemOutcomeProcessed
        Payout.ItemRejected reason detail -> failClaimedOrder po reason detail >> pure ItemOutcomeRejected

parsePayoutRail :: Maybe Text -> Payout.PayoutRail
parsePayoutRail mbRail = case T.toUpper . T.strip <$> mbRail of
  Just "NEFT" -> Payout.RailNEFT
  Just "RTGS" -> Payout.RailRTGS
  Just "IMPS" -> Payout.RailIMPS
  Just "A2A" -> Payout.RailA2A
  _ -> Payout.RailNEFT

payoutRailLabel :: Payout.PayoutRail -> Text
payoutRailLabel = \case
  Payout.RailA2A -> "A2A"
  Payout.RailNEFT -> "NEFT"
  Payout.RailRTGS -> "RTGS"
  Payout.RailIMPS -> "IMPS"

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
    Redis.HedisLTSFlowEnv r
  ) =>
  DSPC.ScheduledPayoutConfig ->
  PR.PayoutType -> -- SCHEDULED or ADHOC
  DTConf.TransporterConfig ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Id DP.Person ->
  Maybe Text ->
  Bool -> -- isManuallyAdded
  m ()
processOneWalletPayout config payoutType transporterConfig merchantId merchantOpCityId personId mbPayoutVpa isManuallyAdded = do
  result <- try $ do
    person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
    let counterparty = counterpartyFromRole person.role
        ctx =
          PayoutContext
            { driverId = personId,
              merchantId = merchantId,
              mocId = merchantOpCityId,
              person = person,
              payoutVpa = mbPayoutVpa,
              transporterConfig = transporterConfig
            }

    Redis.withWaitOnLockRedisWithExpiry (makeWalletRunningBalanceLockKey personId.getId) 10 10 $ do
      now <- getCurrentTime
      mbAccount <- getWalletAccountByOwner counterparty personId.getId
      let mbAccountId = (.id) <$> mbAccount
      walletBalance <- fromMaybe 0 <$> getWalletBalanceByOwner counterparty personId.getId
      -- Single query: get both non-redeemable balance and redeemable entry IDs
      let timeDiff = secondsToNominalDiffTime transporterConfig.timeDiffFromUtc
          cutOffDays = transporterConfig.driverWalletConfig.payoutCutOffDays
          cutoff = payoutCutoffTimeUTC timeDiff cutOffDays now
      (nonRedeemable, redeemableIds, merchantTransferAmt) <- case mbAccountId of
        Nothing -> pure (0, [], 0)
        Just accountId -> getPayoutEligibilityData accountId cutoff now
      let payoutableBalance = walletBalance - nonRedeemable
      logDebug $
        "[SBP-DEBUG] payee=" <> personId.getId
          <> " role="
          <> show person.role
          <> " hasWalletAccount="
          <> show (isJust mbAccountId)
          <> " walletBalance="
          <> show walletBalance
          <> " nonRedeemable="
          <> show nonRedeemable
          <> " payoutableBalance="
          <> show payoutableBalance
          <> " minimum="
          <> show config.minimumPayoutAmount
          <> " isManuallyAdded="
          <> show isManuallyAdded
          <> " willPay="
          <> show (payoutableBalance >= config.minimumPayoutAmount && not isManuallyAdded)

      when (payoutableBalance >= config.minimumPayoutAmount) $ do
        -- Skip manually-added VPAs
        unless isManuallyAdded $ do
          void $ initiateWalletPayout ctx payoutableBalance payoutType Nothing (Just cutoff) (map (.getId) redeemableIds) merchantTransferAmt
  case result of
    Left (e :: SomeException) -> logError $ "ScheduledWalletPayout error for " <> personId.getId <> ": " <> show e
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
