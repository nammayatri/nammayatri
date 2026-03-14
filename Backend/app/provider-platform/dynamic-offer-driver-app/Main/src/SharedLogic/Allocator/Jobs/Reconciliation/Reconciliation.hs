{-
   Copyright 2022-23, Juspay India Pvt Ltd

   This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

   as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

   is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

   or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the

   GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
   -}

module SharedLogic.Allocator.Jobs.Reconciliation.Reconciliation
  ( runReconciliationJob,
  )
where

import Control.Applicative ((<|>))
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as M
import Data.Time.Calendar (addDays)
import Data.Time.Clock (UTCTime (UTCTime), secondsToDiffTime, utctDay)
import qualified Domain.Types.Booking as DB
import qualified Domain.Types.Extra.MerchantPaymentMethod as MP
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.SubscriptionPurchase as DSP
import qualified Domain.Types.TransporterConfig as DTC
import Kernel.Beam.Lib.UtilsTH (HasSchemaName)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Error
import Kernel.Types.Id (Id (..), cast)
import Kernel.Utils.Common
import qualified Lib.Finance.Domain.Types.IndirectTaxTransaction as IndirectTax
import qualified Lib.Finance.Domain.Types.LedgerEntry as LedgerEntry
import qualified Lib.Finance.Domain.Types.ReconciliationEntry as ReconEntry
import qualified Lib.Finance.Domain.Types.ReconciliationSummary as ReconSummary
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Finance.Storage.Queries.IndirectTaxTransactionExtra as QIndirectTax
import qualified Lib.Finance.Storage.Queries.LedgerEntry as QLedger
import qualified Lib.Finance.Storage.Queries.PgPaymentSettlementReportExtra as QPgPaymentSettlement
import qualified Lib.Finance.Storage.Queries.PgPayoutSettlementReportExtra as QPgPayoutSettlement
import qualified Lib.Finance.Storage.Queries.ReconciliationEntry as QReconEntry
import qualified Lib.Finance.Storage.Queries.ReconciliationSummary as QReconSummary
import qualified Lib.Payment.Domain.Types.PayoutRequest as PayoutRequest
import qualified Lib.Payment.Storage.Queries.PayoutRequest as QPayoutRequest
import Lib.Scheduler
import Lib.Scheduler.JobStorageType.DB.Table (SchedulerJobT)
import qualified Lib.Scheduler.JobStorageType.SchedulerType as JC
import SharedLogic.Allocator (AllocatorJobType (..), ReconciliationJobData (..))
import qualified SharedLogic.Finance.Reconciliation as DomainRecon
  ( ReconciliationJobType (..),
    getReconStatusForJob,
    getReconciliationStatus,
    mkReconciliationStatusValue,
    updateReconStatus,
  )
import Storage.Beam.Payment ()
import Storage.Beam.SchedulerJob ()
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.SubscriptionPurchase as QSubPurchase

runReconciliationJob ::
  ( BeamFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m,
    MonadIO m,
    HasShortDurationRetryCfg r c,
    HasField "maxShards" r Int,
    HasField "schedulerSetName" r Text,
    HasField "schedulerType" r SchedulerType,
    HasField "jobInfoMap" r (M.Map Text Bool),
    HasField "blackListedJobs" r [Text],
    JobCreatorEnv r,
    HasSchemaName SchedulerJobT
  ) =>
  Job 'Reconciliation ->
  m ExecutionResult
runReconciliationJob Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
      startTime = jobData.startTime
      endTime = jobData.endTime
      reconciliationTypeText = jobData.reconciliationType
      reconciliationType = parseReconciliationType reconciliationTypeText
      merchantId = jobData.merchantId
      merchantOperatingCityId = jobData.merchantOperatingCityId

  let lockKey = "ReconciliationJob:" <> show merchantId <> ":" <> reconciliationTypeText <> ":" <> show startTime <> ":" <> show endTime

  resultRef <- liftIO $ newIORef Complete
  mbResult <- Hedis.whenWithLockRedisAndReturnValue lockKey 1800 $ do
    -- reducing lock duration @dhruv-1010
    now <- getCurrentTime
    merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
    merchantOpCityId <- CQMOC.getMerchantOpCityId (Just merchantOperatingCityId) merchant Nothing

    transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)

    -- Check if reconciliation is enabled for this merchant
    unless (fromMaybe False transporterConfig.reconciliationJobsEnabled) $ do
      logInfo "Reconciliation job is disabled for this merchant"
      liftIO $ writeIORef resultRef Complete
      return ()

    result <- case reconciliationType of
      ReconSummary.DSR_VS_LEDGER -> doReconciliationDsrVsLedger merchantId merchantOpCityId startTime endTime now
      ReconSummary.DSR_VS_SUBSCRIPTION -> doReconciliationDsrVsSubscription merchantId merchantOpCityId startTime endTime now
      ReconSummary.DSSR_VS_SUBSCRIPTION -> doReconciliationDssrVsSubscription merchantId merchantOpCityId startTime endTime now
      ReconSummary.PG_PAYMENT_SETTLEMENT_VS_SUBSCRIPTION -> doReconciliationPgPaymentVsSubscription merchantId merchantOpCityId startTime endTime now
      ReconSummary.PG_PAYOUT_SETTLEMENT_VS_PAYOUT_REQUEST -> doReconciliationPgPayoutVsPayoutRequest merchantId merchantOpCityId startTime endTime now

    -- Schedule next job for tomorrow at 3:00 AM IST
    scheduleNextReconciliationJob merchantId merchantOperatingCityId transporterConfig reconciliationType

    liftIO $ writeIORef resultRef result
    return ()
  case mbResult of
    Left () -> pure Complete
    Right () -> liftIO $ readIORef resultRef
  where
    parseReconciliationType :: Text -> ReconSummary.ReconciliationType
    parseReconciliationType "DSR_VS_LEDGER" = ReconSummary.DSR_VS_LEDGER
    parseReconciliationType "DSR_VS_SUBSCRIPTION" = ReconSummary.DSR_VS_SUBSCRIPTION
    parseReconciliationType "DSSR_VS_SUBSCRIPTION" = ReconSummary.DSSR_VS_SUBSCRIPTION
    parseReconciliationType "PG_PAYMENT_SETTLEMENT_VS_SUBSCRIPTION" = ReconSummary.PG_PAYMENT_SETTLEMENT_VS_SUBSCRIPTION
    parseReconciliationType "PG_PAYOUT_SETTLEMENT_VS_PAYOUT_REQUEST" = ReconSummary.PG_PAYOUT_SETTLEMENT_VS_PAYOUT_REQUEST
    parseReconciliationType _ = ReconSummary.DSR_VS_LEDGER

    scheduleNextReconciliationJob :: (BeamFlow m r, CacheFlow m r, EsqDBFlow m r, JobCreatorEnv r, HasSchemaName SchedulerJobT, HasField "schedulerType" r SchedulerType) => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DTC.TransporterConfig -> ReconSummary.ReconciliationType -> m ()
    scheduleNextReconciliationJob mId mOpCityId config reconType = do
      now <- getCurrentTime
      -- Calculate tomorrow at 3:00 AM IST (IST = UTC + 5:30)
      let istOffset = secondsToNominalDiffTime config.timeDiffFromUtc
          schedulerTime = fromMaybe 10800 $ fmap (fromIntegral . (.getSeconds)) config.reconciliationSchedulerTime -- default 3:00 AM = 10800 seconds
          todayDay = utctDay now
          yesterdayDay = addDays (-1) todayDay
          tomorrowDay = addDays 1 todayDay
          tomorrow3AMIST = UTCTime tomorrowDay (secondsToDiffTime schedulerTime)
          tomorrow3AMUTC = addUTCTime (negate istOffset) tomorrow3AMIST
          scheduleAfter = diffUTCTime tomorrow3AMUTC now

      when (scheduleAfter > 0) $ do
        let nextJobData =
              ReconciliationJobData
                { merchantId = mId,
                  merchantOperatingCityId = mOpCityId,
                  startTime = UTCTime yesterdayDay (secondsToDiffTime 0),
                  endTime = UTCTime yesterdayDay (secondsToDiffTime 86399),
                  reconciliationType = show reconType
                }
        logInfo $ "Scheduling next reconciliation job for: " <> show nextJobData
        JC.createJobIn @_ @'Reconciliation (Just mId) (Just mOpCityId) scheduleAfter nextJobData
        logInfo "Scheduled next reconciliation job successfully"

-- 1. DSR vs Net Earnings Ledger Reconciliation
doReconciliationDsrVsLedger ::
  ( BeamFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m
  ) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  UTCTime ->
  UTCTime ->
  UTCTime ->
  m ExecutionResult
doReconciliationDsrVsLedger merchantId merchantOpCityId startTime endTime now = do
  logInfo "Starting DSR vs Ledger reconciliation"

  -- Get all completed/cancelled bookings in the date range
  bookings <- QBooking.findAllByStatusAndDateRange merchantOpCityId [DB.COMPLETED, DB.CANCELLED] startTime endTime

  -- Process each booking and update reconciliation status
  entries <- forM bookings $ \booking -> do
    -- Get ledger entries for this booking (multiple reference types) in date range
    ledgerEntries <- ledgerEntriesForBookingInRange booking.id.getId startTime endTime

    -- Process and create reconciliation entry
    mEntry <- processDsrVsLedger booking ledgerEntries now
    return mEntry

  -- Filter valid entries - only include entries where domain reconciliation hasn't already set a status
  -- The domain-level reconciliation now handles the actual status computation
  let validEntries = catMaybes entries

  -- Update booking reconciliation statuses with JSON format for ALL entries (including MATCHED)
  forM_ validEntries $ \entry -> do
    case entry.bookingId of
      Nothing -> pure ()
      Just bookingId -> do
        mbBooking <- QBooking.findById (Id bookingId)
        case mbBooking of
          Nothing -> pure ()
          Just booking -> do
            let existingStatusMap = DomainRecon.getReconciliationStatus booking.reconciliationStatus
            case DomainRecon.getReconStatusForJob existingStatusMap DomainRecon.DSRvsLedger of
              Just _ -> pure ()
              Nothing -> do
                let updatedMap =
                      DomainRecon.updateReconStatus
                        existingStatusMap
                        DomainRecon.DSRvsLedger
                        (toReconSummaryStatus entry.reconStatus)
                    jsonValue = DomainRecon.mkReconciliationStatusValue updatedMap
                QBooking.updateReconciliationStatus (Id bookingId) (Just jsonValue)

  -- Create summary
  summaryId <- cast <$> generateGUID
  let summary = createSummary ReconSummary.DSR_VS_LEDGER validEntries startTime now merchantId.getId merchantOpCityId summaryId
  QReconSummary.create summary

  -- Create entries linked to summary
  let entriesWithSummary = map (\e -> e {ReconEntry.summaryId = summaryId}) validEntries
  mapM_ QReconEntry.create entriesWithSummary

  logInfo $
    "DSR vs Ledger reconciliation completed. Total: " <> show (length validEntries)
      <> ", Matched: "
      <> show (countMatched validEntries)
  return Complete
  where
    countMatched = length . filter (\e -> e.reconStatus == ReconEntry.MATCHED)

-- 2. DSR vs Subscription Transaction Reconciliation
doReconciliationDsrVsSubscription ::
  ( BeamFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m
  ) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  UTCTime ->
  UTCTime ->
  UTCTime ->
  m ExecutionResult
doReconciliationDsrVsSubscription merchantId merchantOpCityId startTime endTime now = do
  logInfo "Starting DSR vs Subscription reconciliation"

  -- Get all completed bookings in date range
  bookings <- QBooking.findAllByStatusAndDateRange merchantOpCityId [DB.COMPLETED] startTime endTime

  -- Process each booking with subscription rides
  entries <- forM bookings $ \booking -> do
    -- Get RideSubscriptionDebit ledger entries in date range
    ledgerEntries <- ledgerEntriesByReferenceInRange "RideSubscriptionDebit" booking.id.getId startTime endTime

    case ledgerEntries of
      [] -> return Nothing -- No subscription debit for this booking
      (ledgerEntry : _) -> do
        entry <- processDsrVsSubscription booking ledgerEntry now
        -- Update booking reconciliation status (map: currentJob : status)
        let existingStatusMap = DomainRecon.getReconciliationStatus booking.reconciliationStatus
            updatedMap = DomainRecon.updateReconStatus existingStatusMap DomainRecon.SubscriptionRecon (toReconSummaryStatus entry.reconStatus)
            jsonValue = DomainRecon.mkReconciliationStatusValue updatedMap
        QBooking.updateReconciliationStatus booking.id (Just jsonValue)
        return $ Just entry

  -- Create summary
  let validEntries = catMaybes entries
  summaryId <- cast <$> generateGUID
  let summary = createSummary ReconSummary.DSR_VS_SUBSCRIPTION validEntries startTime now merchantId.getId merchantOpCityId summaryId
  QReconSummary.create summary

  let entriesWithSummary = map (\e -> e {ReconEntry.summaryId = summaryId}) validEntries
  mapM_ QReconEntry.create entriesWithSummary

  logInfo $ "DSR vs Subscription reconciliation completed. Total: " <> show (length validEntries)
  return Complete

-- 3. DSSR vs Subscription Ledger Reconciliation
doReconciliationDssrVsSubscription ::
  ( BeamFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m
  ) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  UTCTime ->
  UTCTime ->
  UTCTime ->
  m ExecutionResult
doReconciliationDssrVsSubscription merchantId merchantOpCityId startTime endTime now = do
  logInfo "Starting DSSR vs Subscription reconciliation"

  -- Get all active subscription purchases in date range
  subscriptions <- QSubPurchase.findActiveByDateRange merchantOpCityId startTime endTime

  -- Process each subscription
  entries <- forM subscriptions $ \subscription -> do
    -- Get SubscriptionCredit ledger entries in date range (plan credit; purchase-related entries use "SubscriptionPurchase")
    ledgerEntries <- ledgerEntriesByReferenceInRange "SubscriptionCredit" subscription.id.getId startTime endTime

    case ledgerEntries of
      [] -> return Nothing -- No credit entry for this subscription
      (ledgerEntry : _) -> do
        entry <- processDssrVsSubscription subscription ledgerEntry now
        -- Update subscription reconciliation status (map: currentJob : status)
        let existingStatusMap = DomainRecon.getReconciliationStatus subscription.reconciliationStatus
            updatedMap = DomainRecon.updateReconStatus existingStatusMap DomainRecon.DSSRvsSubscription (toReconSummaryStatus entry.reconStatus)
            jsonValue = DomainRecon.mkReconciliationStatusValue updatedMap
        QSubPurchase.updateReconciliationStatus (Just jsonValue) subscription.id
        return $ Just entry

  -- Create summary
  let validEntries = catMaybes entries
  summaryId <- cast <$> generateGUID
  let summary = createSummary ReconSummary.DSSR_VS_SUBSCRIPTION validEntries startTime now merchantId.getId merchantOpCityId summaryId
  QReconSummary.create summary

  let entriesWithSummary = map (\e -> e {ReconEntry.summaryId = summaryId}) validEntries
  mapM_ QReconEntry.create entriesWithSummary

  logInfo $ "DSSR vs Subscription reconciliation completed. Total: " <> show (length validEntries)
  return Complete

-- 4. PG-Payment Settlement Report vs Subscription Purchase Reconciliation
doReconciliationPgPaymentVsSubscription ::
  ( BeamFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m
  ) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  UTCTime ->
  UTCTime ->
  UTCTime ->
  m ExecutionResult
doReconciliationPgPaymentVsSubscription merchantId merchantOpCityId startTime endTime now = do
  logInfo "Starting PG-Payment Settlement vs Subscription reconciliation"

  summaryId <- cast <$> generateGUID

  -- Fetch all SUCCESS pg_payment_settlement_report records in date range
  pgReports <- QPgPaymentSettlement.findByTxnDateRangeAndStatus merchantId.getId merchantOpCityId.getId startTime endTime

  -- Process each settlement report entry
  entries <- forM pgReports $ \report -> do
    -- Match by referenceId = subscription.id; only ACTIVE/EXPIRED subscriptions are valid
    mSubscription <- case report.referenceId of
      Nothing -> return Nothing
      Just refId -> do
        mSub <- QSubPurchase.findByPrimaryKey (Id refId)
        return $
          mSub >>= \s ->
            let okStatus = s.status `elem` [DSP.ACTIVE, DSP.EXPIRED]
                okDate = case report.txnDate of
                  Just txnT -> utctDay txnT == utctDay s.purchaseTimestamp
                  Nothing -> False
             in if okStatus && okDate then Just s else Nothing

    case mSubscription of
      Nothing -> do
        -- No matching active/expired subscription found
        entryId <- generateGUID
        return $
          Just $
            ReconEntry.ReconciliationEntry
              { id = entryId,
                summaryId = summaryId,
                reconciliationDate = now,
                reconciliationType = ReconEntry.PG_PAYMENT_SETTLEMENT_VS_SUBSCRIPTION,
                bookingId = Nothing,
                dcoId = Nothing,
                status = Nothing,
                mode = Nothing,
                expectedDsrValue = 0,
                actualLedgerValue = report.txnAmount,
                variance = report.txnAmount,
                reconStatus = ReconEntry.MISSING_IN_TARGET,
                mismatchReason = Just "No matching subscription purchase found",
                timestamp = now,
                financeComponent = Just ReconEntry.PG_PAYMENT_SETTLEMENT,
                settlementId = report.settlementId,
                sourceId = report.referenceId,
                targetId = Just report.id.getId,
                settlementDate = report.settlementDate,
                transactionDate = report.txnDate,
                rrn = report.rrn,
                settlementMode = fmap show report.paymentMethod,
                sourceDetails = Nothing,
                targetDetails = Nothing,
                merchantId = Just merchantId.getId,
                createdAt = now,
                updatedAt = now,
                merchantOperatingCityId = Just merchantOpCityId.getId
              }
      Just subscription -> do
        -- Compare txnAmount with subscription planFee (includes GST)
        let expectedValue = subscription.planFee
            actualValue = report.txnAmount
            variance = expectedValue - actualValue
            reconStatus =
              if expectedValue == actualValue
                then ReconEntry.MATCHED
                else
                  if actualValue > expectedValue
                    then ReconEntry.HIGHER_IN_TARGET
                    else ReconEntry.LOWER_IN_TARGET

        entryId <- generateGUID

        -- Update subscription reconciliation status
        let existingStatusMap = DomainRecon.getReconciliationStatus subscription.reconciliationStatus
            updatedMap = DomainRecon.updateReconStatus existingStatusMap DomainRecon.PgPaymentVsSubscription (toReconSummaryStatus reconStatus)
            jsonValue = DomainRecon.mkReconciliationStatusValue updatedMap
        QSubPurchase.updateReconciliationStatus (Just jsonValue) subscription.id

        return $
          Just $
            ReconEntry.ReconciliationEntry
              { id = entryId,
                summaryId = summaryId,
                reconciliationDate = now,
                reconciliationType = ReconEntry.PG_PAYMENT_SETTLEMENT_VS_SUBSCRIPTION,
                bookingId = Just subscription.id.getId,
                dcoId = Just subscription.ownerId,
                status = Just ReconEntry.COMPLETED,
                mode = Nothing,
                expectedDsrValue = expectedValue,
                actualLedgerValue = actualValue,
                variance = variance,
                reconStatus = reconStatus,
                mismatchReason = if reconStatus /= ReconEntry.MATCHED then Just "Amount mismatch" else Nothing,
                timestamp = now,
                financeComponent = Just ReconEntry.PG_PAYMENT_SETTLEMENT,
                settlementId = report.settlementId,
                sourceId = Just subscription.id.getId,
                targetId = Just report.id.getId,
                settlementDate = report.settlementDate,
                transactionDate = Just subscription.purchaseTimestamp,
                rrn = report.rrn,
                settlementMode = fmap show report.paymentMethod,
                sourceDetails = Nothing,
                targetDetails = Nothing,
                merchantId = Just merchantId.getId,
                createdAt = now,
                updatedAt = now,
                merchantOperatingCityId = Just merchantOpCityId.getId
              }

  -- Create summary
  let validEntries = catMaybes entries
  let summary = createSummary ReconSummary.PG_PAYMENT_SETTLEMENT_VS_SUBSCRIPTION validEntries startTime now merchantId.getId merchantOpCityId summaryId
  QReconSummary.create summary

  mapM_ QReconEntry.create validEntries

  logInfo $ "PG-Payment Settlement vs Subscription reconciliation completed. Total: " <> show (length validEntries)
  return Complete

-- 5. PG-Payout Settlement Report vs Payout Request Reconciliation
doReconciliationPgPayoutVsPayoutRequest ::
  ( BeamFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m
  ) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  UTCTime ->
  UTCTime ->
  UTCTime ->
  m ExecutionResult
doReconciliationPgPayoutVsPayoutRequest merchantId merchantOpCityId startTime endTime now = do
  logInfo "Starting PG-Payout Settlement vs Payout Request reconciliation"

  summaryId <- cast <$> generateGUID

  -- Fetch all SUCCESS pg_payout_settlement_report records in date range
  pgReports <- QPgPayoutSettlement.findByTxnDateRangeAndStatus merchantId.getId merchantOpCityId.getId startTime endTime

  -- Process each settlement report entry
  entries <- forM pgReports $ \report -> do
    -- Match by payoutRequestId; only CREDITED payout requests are valid
    mPayoutRequest <- case report.payoutRequestId of
      Nothing -> return Nothing
      Just prId -> do
        mPr <- QPayoutRequest.findById (Id prId)
        return $
          mPr >>= \pr ->
            let okStatus = pr.status == PayoutRequest.CREDITED
                okDate = case report.txnDate of
                  Just txnT -> utctDay txnT == utctDay pr.createdAt
                  Nothing -> False
             in if okStatus && okDate then Just pr else Nothing

    case mPayoutRequest of
      Nothing -> do
        -- No matching credited payout request found
        entryId <- generateGUID
        return $
          Just $
            ReconEntry.ReconciliationEntry
              { id = entryId,
                summaryId = summaryId,
                reconciliationDate = now,
                reconciliationType = ReconEntry.PG_PAYOUT_SETTLEMENT_VS_PAYOUT_REQUEST,
                bookingId = Nothing,
                dcoId = Nothing,
                status = Nothing,
                mode = Nothing,
                expectedDsrValue = 0,
                actualLedgerValue = report.txnAmount,
                variance = report.txnAmount,
                reconStatus = ReconEntry.MISSING_IN_TARGET,
                mismatchReason = Just "No matching credited payout request found",
                timestamp = now,
                financeComponent = Just ReconEntry.PG_PAYOUT_SETTLEMENT,
                settlementId = report.settlementId,
                sourceId = report.payoutRequestId,
                targetId = Just report.id.getId,
                settlementDate = report.settlementDate,
                transactionDate = report.txnDate,
                rrn = report.rrn,
                settlementMode = fmap show report.settlementMode,
                sourceDetails = Nothing,
                targetDetails = Nothing,
                merchantId = Just merchantId.getId,
                createdAt = now,
                updatedAt = now,
                merchantOperatingCityId = Just merchantOpCityId.getId
              }
      Just payoutRequest -> do
        -- Compare txnAmount with payoutRequest amount
        let expectedValue = fromMaybe 0 payoutRequest.amount
            actualValue = report.txnAmount
            variance = expectedValue - actualValue
            reconStatus =
              if expectedValue == actualValue
                then ReconEntry.MATCHED
                else
                  if actualValue > expectedValue
                    then ReconEntry.HIGHER_IN_TARGET
                    else ReconEntry.LOWER_IN_TARGET

        entryId <- generateGUID

        return $
          Just $
            ReconEntry.ReconciliationEntry
              { id = entryId,
                summaryId = summaryId,
                reconciliationDate = now,
                reconciliationType = ReconEntry.PG_PAYOUT_SETTLEMENT_VS_PAYOUT_REQUEST,
                bookingId = Just payoutRequest.id.getId,
                dcoId = Just payoutRequest.beneficiaryId,
                status = Just ReconEntry.COMPLETED,
                mode = Nothing,
                expectedDsrValue = expectedValue,
                actualLedgerValue = actualValue,
                variance = variance,
                reconStatus = reconStatus,
                mismatchReason = if reconStatus /= ReconEntry.MATCHED then Just "Amount mismatch" else Nothing,
                timestamp = now,
                financeComponent = Just ReconEntry.PG_PAYOUT_SETTLEMENT,
                settlementId = report.settlementId,
                sourceId = Just payoutRequest.id.getId,
                targetId = Just report.id.getId,
                settlementDate = report.settlementDate,
                transactionDate = Just payoutRequest.createdAt,
                rrn = report.rrn,
                settlementMode = fmap show report.settlementMode,
                sourceDetails = Nothing,
                targetDetails = Nothing,
                merchantId = Just merchantId.getId,
                createdAt = now,
                updatedAt = now,
                merchantOperatingCityId = Just merchantOpCityId.getId
              }

  -- Create summary
  let validEntries = catMaybes entries
  let summary = createSummary ReconSummary.PG_PAYOUT_SETTLEMENT_VS_PAYOUT_REQUEST validEntries startTime now merchantId.getId merchantOpCityId summaryId
  QReconSummary.create summary

  mapM_ QReconEntry.create validEntries

  logInfo $ "PG-Payout Settlement vs Payout Request reconciliation completed. Total: " <> show (length validEntries)
  return Complete

-- | Fetch all ledger entries for a booking (all reference types) within date range
ledgerEntriesForBookingInRange :: (BeamFlow m r, MonadFlow m) => Text -> UTCTime -> UTCTime -> m [LedgerEntry.LedgerEntry]
ledgerEntriesForBookingInRange bookingId startTime endTime = do
  refTypes <- sequence [QLedger.findByReference "BaseRide" bookingId, QLedger.findByReference "GSTOnline" bookingId, QLedger.findByReference "GSTCash" bookingId, QLedger.findByReference "UserCancellation" bookingId, QLedger.findByReference "DriverCancellation" bookingId]
  let allEntries = concat refTypes
  pure $ filter (\e -> e.timestamp >= startTime && e.timestamp <= endTime) allEntries

-- | Fetch ledger entries by referenceType and referenceId in date range
ledgerEntriesByReferenceInRange :: (BeamFlow m r, MonadFlow m) => Text -> Text -> UTCTime -> UTCTime -> m [LedgerEntry.LedgerEntry]
ledgerEntriesByReferenceInRange refType refId startTime endTime = do
  entries <- QLedger.findByReference refType refId
  pure $ filter (\e -> e.timestamp >= startTime && e.timestamp <= endTime) entries

-- Process DSR vs Ledger reconciliation for a single booking
processDsrVsLedger ::
  ( BeamFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m
  ) =>
  DB.Booking ->
  [LedgerEntry.LedgerEntry] ->
  UTCTime ->
  m (Maybe ReconEntry.ReconciliationEntry)
processDsrVsLedger booking ledgerEntries now = do
  let rideMode = determineRideMode booking.paymentInstrument

  -- Look up driver (DCO) from Ride table by bookingId
  rides <- QRide.findRidesByBookingId [booking.id]
  let dcoId = (.driverId.getId) <$> listToMaybe rides

  -- Expected GST from indirect_tax_transaction (referenceId = booking id); use totalGstAmount directly, no recalculation
  indirectTaxTxns <- QIndirectTax.findByReferenceId booking.id.getId
  let relevantType = case booking.status of
        DB.COMPLETED -> IndirectTax.RideFare
        _ -> IndirectTax.Cancellation
      mbIndirectTxn = find (\t -> t.transactionType == relevantType) indirectTaxTxns
      expectedGstFromTax = (.totalGstAmount) <$> mbIndirectTxn
      expectedGstFallback = (* 0.05) <$> calculateExpectedGross booking
      expectedGst = expectedGstFromTax <|> expectedGstFallback

  -- Get expected values from booking (DSR)
  let expectedGross = calculateExpectedGross booking

  -- Find corresponding ledger entries
  let baseRideEntry = find (\e -> e.referenceType == "BaseRide") ledgerEntries
      gstOnlineEntry = find (\e -> e.referenceType == "GSTOnline") ledgerEntries
      gstCashEntry = find (\e -> e.referenceType == "GSTCash") ledgerEntries
      userCancellationEntry = find (\e -> e.referenceType == "UserCancellation") ledgerEntries
      driverCancellationEntry = find (\e -> e.referenceType == "DriverCancellation") ledgerEntries

  -- Determine status based on booking status and payment mode
  let (reconStatusSum, mismatchReason) = case booking.status of
        DB.COMPLETED -> case rideMode of
          Just ReconEntry.ONLINE -> processOnlineCompleted expectedGross baseRideEntry expectedGst gstOnlineEntry
          Just ReconEntry.CASH -> processCashCompleted expectedGst gstCashEntry
          Nothing -> (ReconSummary.MISSING_IN_TARGET, Just "Unknown ride mode")
        DB.CANCELLED -> processCancelled expectedGross userCancellationEntry driverCancellationEntry expectedGst gstOnlineEntry
        _ -> (ReconSummary.MISSING_IN_TARGET, Just "Unsupported booking status")
      reconStatus = toReconEntryStatus reconStatusSum

  -- Calculate variance; for CASH completed we compare GST, so store GST-aligned values
  let actualValue = getActualValue baseRideEntry gstOnlineEntry gstCashEntry userCancellationEntry driverCancellationEntry rideMode booking.status
      (expectedStored, actualStored, variance) =
        case (booking.status, rideMode) of
          (DB.COMPLETED, Just ReconEntry.CASH) ->
            ( fromMaybe 0 expectedGst,
              fromMaybe 0 actualValue,
              maybe 0 (\expG -> expG - fromMaybe 0 actualValue) expectedGst
            )
          _ ->
            ( fromMaybe 0 expectedGross,
              fromMaybe 0 actualValue,
              case (expectedGross, actualValue) of
                (Just expV, Just act) -> expV - act
                _ -> 0
            )

  entryId <- generateGUID

  return $
    Just $
      ReconEntry.ReconciliationEntry
        { id = entryId,
          summaryId = Id "", -- Will be set by caller
          reconciliationDate = now,
          reconciliationType = ReconEntry.DSR_VS_LEDGER,
          bookingId = Just booking.id.getId,
          dcoId = dcoId,
          status = Just $ mapBookingStatus booking.status,
          mode = rideMode,
          expectedDsrValue = expectedStored,
          actualLedgerValue = actualStored,
          variance = variance,
          reconStatus = reconStatus,
          mismatchReason = mismatchReason,
          timestamp = now,
          financeComponent = Just ReconEntry.GROSS_RIDE_FARE,
          sourceDetails = Nothing,
          targetDetails = Nothing,
          merchantId = Just booking.providerId.getId,
          createdAt = now,
          updatedAt = now,
          merchantOperatingCityId = Just booking.merchantOperatingCityId.getId,
          rrn = Nothing,
          settlementDate = Nothing,
          settlementId = Nothing,
          settlementMode = Nothing,
          sourceId = Nothing,
          targetId = Nothing,
          transactionDate = Nothing
        }

-- Process DSR vs Subscription reconciliation
processDsrVsSubscription ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m
  ) =>
  DB.Booking ->
  LedgerEntry.LedgerEntry ->
  UTCTime ->
  m ReconEntry.ReconciliationEntry
processDsrVsSubscription booking ledgerEntry now = do
  -- Look up driver (DCO) from Ride table by bookingId
  rides <- QRide.findRidesByBookingId [booking.id]
  let dcoId = (.driverId.getId) <$> listToMaybe rides

  -- Calculate driver take home: estimatedFare - commission
  let estimatedFare = booking.estimatedFare
      commission = fromMaybe 0 booking.commission
      expectedValue = estimatedFare - commission
      actualValue = ledgerEntry.amount
      variance = expectedValue - actualValue

      reconStatus =
        if expectedValue == actualValue
          then ReconEntry.MATCHED
          else
            if actualValue > expectedValue
              then ReconEntry.HIGHER_IN_TARGET
              else ReconEntry.LOWER_IN_TARGET

  entryId <- generateGUID

  return $
    ReconEntry.ReconciliationEntry
      { id = entryId,
        summaryId = Id "", -- Will be set by caller
        reconciliationDate = now,
        reconciliationType = ReconEntry.DSR_VS_SUBSCRIPTION,
        bookingId = Just booking.id.getId,
        dcoId = dcoId,
        status = Just $ mapBookingStatus booking.status,
        mode = determineRideMode booking.paymentInstrument,
        expectedDsrValue = expectedValue,
        actualLedgerValue = actualValue,
        variance = variance,
        reconStatus = reconStatus,
        mismatchReason = if reconStatus /= ReconEntry.MATCHED then Just "Driver take home mismatch" else Nothing,
        timestamp = now,
        financeComponent = Just ReconEntry.DRIVER_TAKE_HOME_EARNINGS,
        sourceDetails = Nothing,
        targetDetails = Nothing,
        merchantId = Just booking.providerId.getId,
        createdAt = now,
        updatedAt = now,
        merchantOperatingCityId = Just booking.merchantOperatingCityId.getId,
        rrn = Nothing,
        settlementDate = Nothing,
        settlementId = Nothing,
        settlementMode = Nothing,
        sourceId = Nothing,
        targetId = Nothing,
        transactionDate = Nothing
      }

-- Process DSSR vs Subscription reconciliation
processDssrVsSubscription ::
  ( MonadFlow m
  ) =>
  DSP.SubscriptionPurchase ->
  LedgerEntry.LedgerEntry ->
  UTCTime ->
  m ReconEntry.ReconciliationEntry
processDssrVsSubscription subscription ledgerEntry now = do
  let expectedValue = subscription.planRideCredit
      actualValue = ledgerEntry.amount
      variance = expectedValue - actualValue

      reconStatus =
        if expectedValue == actualValue
          then ReconEntry.MATCHED
          else
            if actualValue > expectedValue
              then ReconEntry.HIGHER_IN_TARGET
              else ReconEntry.LOWER_IN_TARGET

  entryId <- generateGUID

  return $
    ReconEntry.ReconciliationEntry
      { id = entryId,
        summaryId = Id "", -- Will be set by caller
        reconciliationDate = now,
        reconciliationType = ReconEntry.DSSR_VS_SUBSCRIPTION,
        bookingId = Just subscription.id.getId,
        dcoId = Just subscription.ownerId,
        status = Just ReconEntry.COMPLETED,
        mode = Nothing,
        expectedDsrValue = expectedValue,
        actualLedgerValue = actualValue,
        variance = variance,
        reconStatus = reconStatus,
        mismatchReason = if reconStatus /= ReconEntry.MATCHED then Just "Subscription credit mismatch" else Nothing,
        timestamp = now,
        financeComponent = Just ReconEntry.SUBSCRIPTION_PURCHASE,
        sourceDetails = Nothing,
        targetDetails = Nothing,
        merchantId = Just subscription.merchantId.getId,
        createdAt = now,
        updatedAt = now,
        merchantOperatingCityId = Just subscription.merchantOperatingCityId.getId,
        rrn = Nothing,
        settlementDate = Nothing,
        settlementId = Nothing,
        settlementMode = Nothing,
        sourceId = Nothing,
        targetId = Nothing,
        transactionDate = Nothing
      }

-- Helper: ReconSummary and ReconEntry both define ReconciliationStatus; convert for entry records.
toReconEntryStatus :: ReconSummary.ReconciliationStatus -> ReconEntry.ReconciliationStatus
toReconEntryStatus ReconSummary.MATCHED = ReconEntry.MATCHED
toReconEntryStatus ReconSummary.HIGHER_IN_TARGET = ReconEntry.HIGHER_IN_TARGET
toReconEntryStatus ReconSummary.LOWER_IN_TARGET = ReconEntry.LOWER_IN_TARGET
toReconEntryStatus ReconSummary.MISSING_IN_TARGET = ReconEntry.MISSING_IN_TARGET

-- Convert back for DB updates (Booking / SubscriptionPurchase store ReconSummary.ReconciliationStatus).
toReconSummaryStatus :: ReconEntry.ReconciliationStatus -> ReconSummary.ReconciliationStatus
toReconSummaryStatus ReconEntry.MATCHED = ReconSummary.MATCHED
toReconSummaryStatus ReconEntry.HIGHER_IN_TARGET = ReconSummary.HIGHER_IN_TARGET
toReconSummaryStatus ReconEntry.LOWER_IN_TARGET = ReconSummary.LOWER_IN_TARGET
toReconSummaryStatus ReconEntry.MISSING_IN_TARGET = ReconSummary.MISSING_IN_TARGET

-- Helper functions

determineRideMode :: Maybe MP.PaymentInstrument -> Maybe ReconEntry.RideMode
determineRideMode (Just MP.Cash) = Just ReconEntry.CASH
determineRideMode (Just (MP.Card _)) = Just ReconEntry.ONLINE
determineRideMode (Just (MP.Wallet _)) = Just ReconEntry.ONLINE
determineRideMode (Just MP.UPI) = Just ReconEntry.ONLINE
determineRideMode (Just MP.BoothOnline) = Just ReconEntry.ONLINE
determineRideMode (Just MP.NetBanking) = Just ReconEntry.ONLINE
determineRideMode _ = Nothing

mapBookingStatus :: DB.BookingStatus -> ReconEntry.RideStatus
mapBookingStatus DB.COMPLETED = ReconEntry.COMPLETED
mapBookingStatus DB.CANCELLED = ReconEntry.CANCELLED
mapBookingStatus _ = ReconEntry.CANCELLED

calculateExpectedGross :: DB.Booking -> Maybe HighPrecMoney
calculateExpectedGross booking =
  let estimatedFare = booking.estimatedFare
      tollCharges = fromMaybe 0 booking.tollCharges
   in Just $ estimatedFare - tollCharges

-- | HIGHER_IN_TARGET when actual > expected, LOWER_IN_TARGET when actual < expected (per doc semantics).
mismatchStatus :: Maybe HighPrecMoney -> Maybe HighPrecMoney -> ReconSummary.ReconciliationStatus
mismatchStatus expected actual = case (expected, actual) of
  (Just e, Just a) | a > e -> ReconSummary.HIGHER_IN_TARGET
  (Just _e, Just _a) -> ReconSummary.LOWER_IN_TARGET
  _ -> ReconSummary.LOWER_IN_TARGET

processOnlineCompleted :: Maybe HighPrecMoney -> Maybe LedgerEntry.LedgerEntry -> Maybe HighPrecMoney -> Maybe LedgerEntry.LedgerEntry -> (ReconSummary.ReconciliationStatus, Maybe Text)
processOnlineCompleted expectedGross baseRideEntry expectedGst gstOnlineEntry =
  let actualGross = baseRideEntry <&> (.amount)
      actualGst = gstOnlineEntry <&> (.amount)
      grossMatch = case (expectedGross, actualGross) of
        (Just expV, Just act) -> expV == act
        _ -> False
      gstMatch = case (expectedGst, actualGst) of
        (Just expV, Just act) -> expV == act
        _ -> False
   in if grossMatch && gstMatch
        then (ReconSummary.MATCHED, Nothing)
        else
          if not grossMatch
            then (mismatchStatus expectedGross actualGross, Just "BaseRide mismatch - expected vs actual gross differs")
            else (mismatchStatus expectedGst actualGst, Just "GST mismatch - expected vs actual GST differs")

processCashCompleted :: Maybe HighPrecMoney -> Maybe LedgerEntry.LedgerEntry -> (ReconSummary.ReconciliationStatus, Maybe Text)
processCashCompleted expectedGst gstCashEntry =
  case (expectedGst, gstCashEntry <&> (.amount)) of
    (Just expV, Just act) ->
      if expV == act
        then (ReconSummary.MATCHED, Nothing)
        else (mismatchStatus (Just expV) (Just act), Just "GSTCash mismatch")
    (Nothing, Just _) -> (ReconSummary.MISSING_IN_TARGET, Just "Expected GST missing for cash ride")
    (Just _, Nothing) -> (ReconSummary.MISSING_IN_TARGET, Just "GSTCash entry missing in ledger")
    _ -> (ReconSummary.MISSING_IN_TARGET, Just "Both expected and actual GST missing")

processCancelled :: Maybe HighPrecMoney -> Maybe LedgerEntry.LedgerEntry -> Maybe LedgerEntry.LedgerEntry -> Maybe HighPrecMoney -> Maybe LedgerEntry.LedgerEntry -> (ReconSummary.ReconciliationStatus, Maybe Text)
processCancelled expectedGross userCancellationEntry driverCancellationEntry expectedGst gstOnlineEntry =
  case (userCancellationEntry <&> (.amount), driverCancellationEntry <&> (.amount)) of
    (Just userAmt, _) -> case expectedGross of
      Just expV ->
        if expV == userAmt
          then (ReconSummary.MATCHED, Nothing)
          else (mismatchStatus (Just expV) (Just userAmt), Just "User cancellation amount mismatch")
      Nothing -> (ReconSummary.MISSING_IN_TARGET, Just "Expected value missing")
    (_, Just driverAmt) -> case (expectedGross, expectedGst, gstOnlineEntry <&> (.amount)) of
      (Just expGross, Just expGst, Just actGst) ->
        if expGross == driverAmt && expGst == actGst
          then (ReconSummary.MATCHED, Nothing)
          else
            if expGross /= driverAmt
              then (mismatchStatus (Just expGross) (Just driverAmt), Just "Driver cancellation amount mismatch")
              else (mismatchStatus (Just expGst) (Just actGst), Just "GST mismatch for driver cancellation")
      (Nothing, _, _) -> (ReconSummary.MISSING_IN_TARGET, Just "Expected gross missing")
      (_, Nothing, _) -> (ReconSummary.MISSING_IN_TARGET, Just "Expected GST missing")
      (_, _, Nothing) -> (ReconSummary.MISSING_IN_TARGET, Just "GSTOnline entry missing")
    _ -> (ReconSummary.MISSING_IN_TARGET, Just "No cancellation entry found")

getActualValue :: Maybe LedgerEntry.LedgerEntry -> Maybe LedgerEntry.LedgerEntry -> Maybe LedgerEntry.LedgerEntry -> Maybe LedgerEntry.LedgerEntry -> Maybe LedgerEntry.LedgerEntry -> Maybe ReconEntry.RideMode -> DB.BookingStatus -> Maybe HighPrecMoney
getActualValue baseRideEntry _gstOnlineEntry gstCashEntry userCancellationEntry driverCancellationEntry mode status =
  case status of
    DB.COMPLETED -> case mode of
      Just ReconEntry.ONLINE -> baseRideEntry <&> (.amount)
      Just ReconEntry.CASH -> gstCashEntry <&> (.amount)
      _ -> Nothing
    DB.CANCELLED -> (userCancellationEntry <&> (.amount)) <|> (driverCancellationEntry <&> (.amount))
    _ -> Nothing

createSummary :: ReconSummary.ReconciliationType -> [ReconEntry.ReconciliationEntry] -> UTCTime -> UTCTime -> Text -> Id DMOC.MerchantOperatingCity -> Id ReconSummary.ReconciliationSummary -> ReconSummary.ReconciliationSummary
createSummary reconType entries startTime now merchantId merchantOpCityId summaryId =
  let totalRecords = length entries
      matchedRecords = length $ filter (\e -> e.reconStatus == ReconEntry.MATCHED) entries
      discrepancies = totalRecords - matchedRecords
      sourceTotal = sum $ map (.expectedDsrValue) entries
      targetTotal = sum $ map (.actualLedgerValue) entries
      variance = sourceTotal - targetTotal
      matchRate = if totalRecords > 0 then show (fromIntegral matchedRecords * 100 / fromIntegral totalRecords :: Double) <> "%" else "0%"
   in ReconSummary.ReconciliationSummary
        { id = summaryId,
          reconciliationDate = startTime,
          reconciliationType = reconType,
          merchantId = merchantId,
          merchantOperatingCityId = merchantOpCityId.getId,
          totalDiscrepancies = discrepancies,
          matchedRecords = matchedRecords,
          matchRate = matchRate,
          sourceTotal = sourceTotal,
          targetTotal = targetTotal,
          varianceAmount = variance,
          status = ReconSummary.COMPLETED,
          errorMessage = Nothing,
          createdAt = now,
          updatedAt = now
        }
