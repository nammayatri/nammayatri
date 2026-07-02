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
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Plan as DPlan
import qualified Domain.Types.ScheduledPayout as ScheduledPayout
import qualified Domain.Types.SubscriptionPurchase as DSP
import qualified Domain.Types.TransporterConfig as DTC
import Kernel.Beam.Lib.UtilsTH (HasSchemaName)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Error
import Kernel.Types.Id (Id (..), cast)
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified Lib.Finance.Domain.Types.DirectTaxTransaction as DirectTax
import qualified Lib.Finance.Domain.Types.IndirectTaxTransaction as IndirectTax
import qualified Lib.Finance.Domain.Types.LedgerEntry as LedgerEntry
import qualified Lib.Finance.Domain.Types.ReconciliationEntry as ReconEntry
import qualified Lib.Finance.Domain.Types.ReconciliationSummary as ReconSummary
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Finance.Storage.Queries.DirectTaxTransaction as QDirectTax
import qualified Lib.Finance.Storage.Queries.IndirectTaxTransaction as QIndirectTax
import qualified Lib.Finance.Storage.Queries.LedgerEntry as QLedger
import qualified Lib.Finance.Storage.Queries.LedgerEntryExtra as QLedgerExtra
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
import SharedLogic.Finance.Prepaid
  ( expiryCreditTransferReferenceType,
    prepaidRideDebitReferenceType,
    subscriptionCreditReferenceType,
  )
import qualified SharedLogic.Finance.Reconciliation as DomainRecon
  ( ReconEntryInput (..),
    ReconciliationJobType (..),
    determineRideMode,
    dsrVsLedgerRefTypes,
    getReconStatusForJob,
    getReconciliationStatus,
    mapBookingStatus,
    mkDefaultReconEntryInput,
    mkReconEntry,
    mkReconciliationStatusValue,
    reasonAmountMismatch,
    reasonDriverTakeHomeMismatch,
    reasonMissingSubscriptionTransaction,
    reasonNoMatchingPayoutRequest,
    reasonNoMatchingSubscription,
    reasonOverSubscriptionConsumption,
    reasonPartialSubscriptionConsumption,
    reasonSubscriptionCreditMismatch,
    runDsrVsLedgerComparison,
    subscriptionIndirectTaxTxn,
    subscriptionPurchaseGstAmounts,
    updateReconStatus,
  )
import Storage.Beam.Payment ()
import Storage.Beam.SchedulerJob ()
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Plan as CQPlan
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.FareParameters as QFareParams
import qualified Storage.Queries.Plan as QPlan
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.ScheduledPayout as QScheduledPayout
import qualified Storage.Queries.SubscriptionPurchase as QSubPurchase
import qualified Storage.Queries.SubscriptionPurchaseExtra as QSubPurchaseExtra

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

    transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId}) (Just (SCTC.findByMerchantOpCityId merchantOpCityId Nothing)) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)

    -- Check if reconciliation is enabled for this merchant
    unless (fromMaybe False transporterConfig.reconciliationJobsEnabled) $ do
      logInfo "Reconciliation job is disabled for this merchant"
      liftIO $ writeIORef resultRef Complete
      return ()

    result <- case reconciliationType of
      ReconSummary.DSR_VS_LEDGER -> doReconciliationDsrVsLedger merchantId merchantOpCityId startTime endTime now
      ReconSummary.DSR_VS_SUBSCRIPTION -> doReconciliationDsrVsSubscription merchantId merchantOpCityId startTime endTime now
      ReconSummary.DSSR_VS_SUBSCRIPTION -> doReconciliationDssrVsSubscription merchantId merchantOpCityId startTime endTime now
      ReconSummary.SUBSCRIPTION_PURCHASE_VS_SUBSCRIPTION_TRANSACTION -> doReconciliationPurchaseVsSubscriptionTransaction merchantId merchantOpCityId startTime endTime now
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
    parseReconciliationType "SUBSCRIPTION_PURCHASE_VS_SUBSCRIPTION_TRANSACTION" = ReconSummary.SUBSCRIPTION_PURCHASE_VS_SUBSCRIPTION_TRANSACTION
    parseReconciliationType "PG_PAYMENT_SETTLEMENT_VS_SUBSCRIPTION" = ReconSummary.PG_PAYMENT_SETTLEMENT_VS_SUBSCRIPTION
    parseReconciliationType "PG_PAYOUT_SETTLEMENT_VS_PAYOUT_REQUEST" = ReconSummary.PG_PAYOUT_SETTLEMENT_VS_PAYOUT_REQUEST
    parseReconciliationType _ = ReconSummary.DSR_VS_LEDGER

    scheduleNextReconciliationJob :: (BeamFlow m r, CacheFlow m r, EsqDBFlow m r, JobCreatorEnv r, HasSchemaName SchedulerJobT, HasField "schedulerType" r SchedulerType) => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DTC.TransporterConfig -> ReconSummary.ReconciliationType -> m ()
    scheduleNextReconciliationJob mId mOpCityId config reconType = do
      now <- getCurrentTime
      -- Calculate tomorrow at 3:00 AM IST (IST = UTC + 5:30)
      let istOffset = secondsToNominalDiffTime config.timeDiffFromUtc
          schedulerTime = fromMaybe 10800 $ fmap (fromIntegral . (.getSeconds)) config.reconciliationSchedulerTime -- default 3:00 AM = 10800 seconds
          nowIST = addUTCTime istOffset now
          todayDayIST = utctDay nowIST
          tomorrowDayIST = addDays 1 todayDayIST
          tomorrow3AMIST = UTCTime tomorrowDayIST (secondsToDiffTime schedulerTime)
          tomorrow3AMUTC = addUTCTime (negate istOffset) tomorrow3AMIST
          scheduleAfter = diffUTCTime tomorrow3AMUTC now

      when (scheduleAfter > 0) $ do
        let nextJobData =
              ReconciliationJobData
                { merchantId = mId,
                  merchantOperatingCityId = mOpCityId,
                  startTime = UTCTime todayDayIST (secondsToDiffTime 0),
                  endTime = UTCTime todayDayIST (secondsToDiffTime 86399),
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
    ledgerEntries <- ledgerEntriesForBookingInRange booking.id.getId startTime endTime
    indirectTaxTxns <- QIndirectTax.findByReferenceId booking.id.getId
    directTaxTxns <- QDirectTax.findByReferenceId booking.id.getId
    mEntry <- processDsrVsLedger booking ledgerEntries indirectTaxTxns directTaxTxns now
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

  -- Persist summaries and entries
  saveSummaryAndEntries ReconSummary.DSR_VS_LEDGER validEntries startTime now merchantId.getId merchantOpCityId

  logInfo $
    "DSR vs Ledger reconciliation completed. Total: " <> show (length validEntries)
      <> ", Matched: "
      <> show (countMatched validEntries)
  return Complete
  where
    countMatched = length . filter (\e -> e.reconStatus == ReconEntry.MATCHED)

-- | Shared helper: create summary, persist entries, link to summary.
saveSummaryAndEntries ::
  (BeamFlow m r, MonadFlow m) =>
  ReconSummary.ReconciliationType ->
  [ReconEntry.ReconciliationEntry] ->
  UTCTime ->
  UTCTime ->
  Text ->
  Id DMOC.MerchantOperatingCity ->
  m ()
saveSummaryAndEntries reconType entries startTime now merchantId merchantOpCityId = do
  summaryId <- cast <$> generateGUID
  let summary = createSummary reconType entries startTime now merchantId merchantOpCityId summaryId
  QReconSummary.create summary
  let entriesWithSummary = map (\e -> e {ReconEntry.summaryId = summaryId}) entries
  mapM_ QReconEntry.create entriesWithSummary

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
    ledgerEntries <- QLedger.findByReference prepaidRideDebitReferenceType booking.id.getId

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

  -- Persist summaries and entries
  let validEntries = catMaybes entries
  saveSummaryAndEntries ReconSummary.DSR_VS_SUBSCRIPTION validEntries startTime now merchantId.getId merchantOpCityId

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
    ledgerEntries <- QLedger.findByReference subscriptionCreditReferenceType subscription.id.getId

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

  -- Persist summaries and entries
  let validEntries = catMaybes entries
  saveSummaryAndEntries ReconSummary.DSSR_VS_SUBSCRIPTION validEntries startTime now merchantId.getId merchantOpCityId

  logInfo $ "DSSR vs Subscription reconciliation completed. Total: " <> show (length validEntries)
  return Complete

-- 4. Subscription Purchase vs Subscription Transaction Reconciliation (Block 2)
doReconciliationPurchaseVsSubscriptionTransaction ::
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
doReconciliationPurchaseVsSubscriptionTransaction merchantId merchantOpCityId startTime endTime now = do
  logInfo "Starting Subscription Purchase vs Subscription Transaction reconciliation"

  subscriptions <- QSubPurchaseExtra.findPurchasedByDateRange merchantOpCityId startTime endTime

  entries <- forM subscriptions $ \subscription -> do
    entry <- processPurchaseVsSubscriptionTransaction subscription now
    let existingStatusMap = DomainRecon.getReconciliationStatus subscription.reconciliationStatus
        updatedMap =
          DomainRecon.updateReconStatus
            existingStatusMap
            DomainRecon.PurchaseVsSubscriptionTransaction
            (toReconSummaryStatus entry.reconStatus)
        jsonValue = DomainRecon.mkReconciliationStatusValue updatedMap
    QSubPurchase.updateReconciliationStatus (Just jsonValue) subscription.id
    pure $ Just entry

  let validEntries = catMaybes entries
  saveSummaryAndEntries ReconSummary.SUBSCRIPTION_PURCHASE_VS_SUBSCRIPTION_TRANSACTION validEntries startTime now merchantId.getId merchantOpCityId

  logInfo $
    "Subscription Purchase vs Subscription Transaction reconciliation completed. Total: "
      <> show (length validEntries)
  return Complete

-- 5. PG-Payment Settlement Report vs Subscription Purchase Reconciliation
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

  pgReports <- QPgPaymentSettlement.findByTxnDateRangeAndStatus merchantId.getId merchantOpCityId.getId startTime endTime

  entries <- forM pgReports $ \report -> do
    mSubscription <- case report.referenceId of
      Nothing -> return Nothing
      Just refId -> do
        mSub <- QSubPurchase.findByPrimaryKey (Id refId)
        return $
          mSub >>= \s ->
            let okStatus = s.status `elem` [DSP.ACTIVE, DSP.EXPIRED, DSP.EXHAUSTED]
                okDate = case report.txnDate of
                  Just txnT -> utctDay txnT == utctDay s.purchaseTimestamp
                  Nothing -> False
             in if okStatus && okDate then Just s else Nothing

    entryId <- generateGUID

    case mSubscription of
      Nothing -> do
        let inp =
              (DomainRecon.mkDefaultReconEntryInput ReconEntry.PG_PAYMENT_SETTLEMENT_VS_SUBSCRIPTION)
                { DomainRecon.actual = report.txnAmount,
                  DomainRecon.reason = Just DomainRecon.reasonNoMatchingSubscription,
                  DomainRecon.component = Just ReconEntry.PG_PAYMENT_SETTLEMENT,
                  DomainRecon.settlementId = report.settlementId,
                  DomainRecon.sourceId = report.referenceId,
                  DomainRecon.targetId = Just report.id.getId,
                  DomainRecon.settlementDate = report.settlementDate,
                  DomainRecon.transactionDate = report.txnDate,
                  DomainRecon.pgTransactionDate = report.txnDate,
                  DomainRecon.rrn = report.rrn,
                  DomainRecon.utr = report.utr,
                  DomainRecon.settlementMode = fmap show report.paymentMethod,
                  DomainRecon.pgOrderId = Just report.orderId,
                  DomainRecon.pgTxnId = report.txnId,
                  DomainRecon.merchantId = Just merchantId.getId,
                  DomainRecon.merchantOperatingCityId = Just merchantOpCityId.getId
                }
        return $ Just $ DomainRecon.mkReconEntry inp now entryId
      Just subscription -> do
        indirectTaxTxns <- QIndirectTax.findByReferenceId subscription.id.getId
        let gstAmounts =
              DomainRecon.subscriptionPurchaseGstAmounts subscription.planFee $
                DomainRecon.subscriptionIndirectTaxTxn indirectTaxTxns
            inp =
              (DomainRecon.mkDefaultReconEntryInput ReconEntry.PG_PAYMENT_SETTLEMENT_VS_SUBSCRIPTION)
                { DomainRecon.bookingId = Just subscription.id.getId,
                  DomainRecon.dcoId = Just subscription.ownerId,
                  DomainRecon.status = Just ReconEntry.COMPLETED,
                  DomainRecon.expected = subscription.planFee,
                  DomainRecon.actual = report.txnAmount,
                  DomainRecon.reason = Just DomainRecon.reasonAmountMismatch,
                  DomainRecon.component = Just ReconEntry.PG_PAYMENT_SETTLEMENT,
                  DomainRecon.settlementId = report.settlementId,
                  DomainRecon.sourceId = Just subscription.id.getId,
                  DomainRecon.targetId = Just report.id.getId,
                  DomainRecon.settlementDate = report.settlementDate,
                  DomainRecon.transactionDate = Just subscription.purchaseTimestamp,
                  DomainRecon.pgTransactionDate = report.txnDate,
                  DomainRecon.rrn = report.rrn,
                  DomainRecon.utr = report.utr,
                  DomainRecon.settlementMode = fmap show report.paymentMethod,
                  DomainRecon.pgOrderId = Just report.orderId,
                  DomainRecon.pgTxnId = report.txnId,
                  DomainRecon.paymentOrderId = Just subscription.paymentOrderId.getId,
                  DomainRecon.subscriptionAmountExclGst = Just gstAmounts.amountExclGst,
                  DomainRecon.gstOnSubscription = Just gstAmounts.gst,
                  DomainRecon.totalTransactionAmount = Just gstAmounts.totalInclGst,
                  DomainRecon.merchantId = Just merchantId.getId,
                  DomainRecon.merchantOperatingCityId = Just merchantOpCityId.getId
                }
            entry = DomainRecon.mkReconEntry inp now entryId

        -- Update subscription reconciliation status
        let existingStatusMap = DomainRecon.getReconciliationStatus subscription.reconciliationStatus
            updatedMap = DomainRecon.updateReconStatus existingStatusMap DomainRecon.PgPaymentVsSubscription (toReconSummaryStatus entry.reconStatus)
            jsonValue = DomainRecon.mkReconciliationStatusValue updatedMap
        QSubPurchase.updateReconciliationStatus (Just jsonValue) subscription.id

        return $ Just entry

  let validEntries = catMaybes entries
  saveSummaryAndEntries ReconSummary.PG_PAYMENT_SETTLEMENT_VS_SUBSCRIPTION validEntries startTime now merchantId.getId merchantOpCityId

  logInfo $ "PG-Payment Settlement vs Subscription reconciliation completed. Total: " <> show (length validEntries)
  return Complete

-- 6. PG-Payout Settlement Report vs Payout Request Reconciliation
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

  pgReports <- QPgPayoutSettlement.findByTxnDateRangeAndStatus merchantId.getId merchantOpCityId.getId startTime endTime

  entries <- forM pgReports $ \report -> do
    mPayoutRequest <- case report.payoutRequestId of
      Nothing -> return Nothing
      Just prId -> do
        mPr <- QPayoutRequest.findById (Id prId)
        mResolvedPr <- case mPr of
          Just pr -> pure (Just pr)
          Nothing -> do
            mSp <- QScheduledPayout.findById (Id prId)
            pure $ fmap scheduledPayoutToPayoutRequest mSp
        return $
          mResolvedPr >>= \pr ->
            let okStatus = pr.status == PayoutRequest.CREDITED
                okDate = case report.txnDate of
                  Just txnT -> utctDay txnT == utctDay pr.createdAt
                  Nothing -> False
             in if okStatus && okDate then Just pr else Nothing

    entryId <- generateGUID

    case mPayoutRequest of
      Nothing -> do
        let inp =
              (DomainRecon.mkDefaultReconEntryInput ReconEntry.PG_PAYOUT_SETTLEMENT_VS_PAYOUT_REQUEST)
                { DomainRecon.actual = report.txnAmount,
                  DomainRecon.reason = Just DomainRecon.reasonNoMatchingPayoutRequest,
                  DomainRecon.component = Just ReconEntry.PG_PAYOUT_SETTLEMENT,
                  DomainRecon.settlementId = report.settlementId,
                  DomainRecon.sourceId = report.payoutRequestId,
                  DomainRecon.targetId = Just report.id.getId,
                  DomainRecon.settlementDate = report.settlementDate,
                  DomainRecon.transactionDate = report.txnDate,
                  DomainRecon.rrn = report.rrn,
                  DomainRecon.settlementMode = fmap show report.settlementMode,
                  DomainRecon.merchantId = Just merchantId.getId,
                  DomainRecon.merchantOperatingCityId = Just merchantOpCityId.getId
                }
        return $ Just $ DomainRecon.mkReconEntry inp now entryId
      Just payoutRequest -> do
        let inp =
              (DomainRecon.mkDefaultReconEntryInput ReconEntry.PG_PAYOUT_SETTLEMENT_VS_PAYOUT_REQUEST)
                { DomainRecon.bookingId = Just payoutRequest.id.getId,
                  DomainRecon.dcoId = Just payoutRequest.beneficiaryId,
                  DomainRecon.status = Just ReconEntry.COMPLETED,
                  DomainRecon.expected = fromMaybe 0 payoutRequest.amount,
                  DomainRecon.actual = report.txnAmount,
                  DomainRecon.reason = Just DomainRecon.reasonAmountMismatch,
                  DomainRecon.component = Just ReconEntry.PG_PAYOUT_SETTLEMENT,
                  DomainRecon.settlementId = report.settlementId,
                  DomainRecon.sourceId = Just payoutRequest.id.getId,
                  DomainRecon.targetId = Just report.id.getId,
                  DomainRecon.settlementDate = report.settlementDate,
                  DomainRecon.transactionDate = Just payoutRequest.createdAt,
                  DomainRecon.rrn = report.rrn,
                  DomainRecon.settlementMode = fmap show report.settlementMode,
                  DomainRecon.merchantId = Just merchantId.getId,
                  DomainRecon.merchantOperatingCityId = Just merchantOpCityId.getId
                }
        return $ Just $ DomainRecon.mkReconEntry inp now entryId

  let validEntries = catMaybes entries
  saveSummaryAndEntries ReconSummary.PG_PAYOUT_SETTLEMENT_VS_PAYOUT_REQUEST validEntries startTime now merchantId.getId merchantOpCityId

  logInfo $ "PG-Payout Settlement vs Payout Request reconciliation completed. Total: " <> show (length validEntries)
  return Complete

-- | Fetch all ledger entries for a booking (all reference types) within date range
ledgerEntriesForBookingInRange :: (BeamFlow m r, MonadFlow m) => Text -> UTCTime -> UTCTime -> m [LedgerEntry.LedgerEntry]
ledgerEntriesForBookingInRange bookingId startTime endTime = do
  allEntries <- QLedgerExtra.findByReferenceIn DomainRecon.dsrVsLedgerRefTypes bookingId
  pure $ filter (\e -> e.timestamp >= startTime && e.timestamp <= endTime) allEntries

-- Process DSR vs Ledger reconciliation for a single booking
processDsrVsLedger ::
  ( BeamFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m
  ) =>
  DB.Booking ->
  [LedgerEntry.LedgerEntry] ->
  [IndirectTax.IndirectTaxTransaction] ->
  [DirectTax.DirectTaxTransaction] ->
  UTCTime ->
  m (Maybe ReconEntry.ReconciliationEntry)
processDsrVsLedger booking ledgerEntries indirectTaxTxns directTaxTxns now = do
  rides <- QRide.findRidesByBookingId [booking.id]
  let dcoId = (.driverId.getId) <$> listToMaybe rides
      mbRide = listToMaybe rides
  rideFareParams <- maybe (pure Nothing) (\ride -> maybe (pure Nothing) (QFareParams.findById) ride.fareParametersId) mbRide
  let dsrResult = DomainRecon.runDsrVsLedgerComparison booking mbRide rideFareParams ledgerEntries indirectTaxTxns directTaxTxns

  entryId <- generateGUID
  let inp =
        (DomainRecon.mkDefaultReconEntryInput ReconEntry.DSR_VS_LEDGER)
          { DomainRecon.bookingId = Just booking.id.getId,
            DomainRecon.dcoId = dcoId,
            DomainRecon.status = Just $ DomainRecon.mapBookingStatus booking.status,
            DomainRecon.mode = dsrResult.rideMode,
            DomainRecon.expected = dsrResult.expectedStored,
            DomainRecon.actual = dsrResult.actualStored,
            DomainRecon.reason = dsrResult.mismatchReason,
            DomainRecon.component = Just dsrResult.financeComponent,
            DomainRecon.merchantId = Just booking.providerId.getId,
            DomainRecon.merchantOperatingCityId = Just booking.merchantOperatingCityId.getId
          }
      -- Override reconStatus with the multi-component comparison result
      entry =
        (DomainRecon.mkReconEntry inp now entryId)
          { ReconEntry.reconStatus = toReconEntryStatus dsrResult.reconStatus,
            ReconEntry.mismatchReason = dsrResult.mismatchReason
          }
  return $ Just entry

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
  rides <- QRide.findRidesByBookingId [booking.id]
  let dcoId = (.driverId.getId) <$> listToMaybe rides
      mbRide = listToMaybe rides
  rideFareParams <- maybe (pure Nothing) (\ride -> maybe (pure Nothing) (QFareParams.findById) ride.fareParametersId) mbRide
  let fareParams = fromMaybe booking.fareParams rideFareParams
      rideFare = fromMaybe booking.estimatedFare (mbRide >>= (.fare))
      parkingCharge = fromMaybe 0 fareParams.parkingCharge
      govtCharges = fromMaybe 0 fareParams.govtCharges
      tollCharges = fromMaybe 0 $ (mbRide >>= (.tollCharges)) <|> booking.tollCharges
      expectedValue = rideFare - parkingCharge - govtCharges - tollCharges

  entryId <- generateGUID
  let inp =
        (DomainRecon.mkDefaultReconEntryInput ReconEntry.DSR_VS_SUBSCRIPTION)
          { DomainRecon.bookingId = Just booking.id.getId,
            DomainRecon.dcoId = dcoId,
            DomainRecon.status = Just $ DomainRecon.mapBookingStatus booking.status,
            DomainRecon.mode = DomainRecon.determineRideMode booking.ledgerWriteMode booking.paymentInstrument,
            DomainRecon.expected = expectedValue,
            DomainRecon.actual = ledgerEntry.amount,
            DomainRecon.reason = Just DomainRecon.reasonDriverTakeHomeMismatch,
            DomainRecon.component = Just ReconEntry.DRIVER_TAKE_HOME_EARNINGS,
            DomainRecon.merchantId = Just booking.providerId.getId,
            DomainRecon.merchantOperatingCityId = Just booking.merchantOperatingCityId.getId
          }
  return $ DomainRecon.mkReconEntry inp now entryId

-- Process DSSR vs Subscription reconciliation
processDssrVsSubscription ::
  ( MonadFlow m
  ) =>
  DSP.SubscriptionPurchase ->
  LedgerEntry.LedgerEntry ->
  UTCTime ->
  m ReconEntry.ReconciliationEntry
processDssrVsSubscription subscription ledgerEntry now = do
  entryId <- generateGUID
  let inp =
        (DomainRecon.mkDefaultReconEntryInput ReconEntry.DSSR_VS_SUBSCRIPTION)
          { DomainRecon.bookingId = Just subscription.id.getId,
            DomainRecon.dcoId = Just subscription.ownerId,
            DomainRecon.status = Just ReconEntry.COMPLETED,
            DomainRecon.expected = subscription.planRideCredit,
            DomainRecon.actual = ledgerEntry.amount,
            DomainRecon.reason = Just DomainRecon.reasonSubscriptionCreditMismatch,
            DomainRecon.component = Just ReconEntry.SUBSCRIPTION_PURCHASE,
            DomainRecon.merchantId = Just subscription.merchantId.getId,
            DomainRecon.merchantOperatingCityId = Just subscription.merchantOperatingCityId.getId
          }
  return $ DomainRecon.mkReconEntry inp now entryId

-- | Sum ride debits (linked bookings) and expiry transfers for a subscription purchase.
calculateSubscriptionConsumedAmount ::
  ( BeamFlow m r,
    EsqDBFlow m r,
    MonadFlow m,
    CacheFlow m r
  ) =>
  DSP.SubscriptionPurchase ->
  m HighPrecMoney
calculateSubscriptionConsumedAmount subscription = do
  rides <- QRide.findAllBySubscriptionPurchaseId subscription.id
  rideDebitAmount <-
    fmap sum $
      forM rides $ \ride -> do
        entries <- QLedger.findByReference prepaidRideDebitReferenceType ride.bookingId.getId
        pure $ sum $ map (.amount) $ filter ((== LedgerEntry.SETTLED) . (.status)) entries
  expiryEntries <- QLedger.findByReference expiryCreditTransferReferenceType subscription.id.getId
  let expiryAmount = sum $ map (.amount) $ filter ((== LedgerEntry.SETTLED) . (.status)) expiryEntries
  pure (rideDebitAmount + expiryAmount)

-- Process Subscription Purchase vs Subscription Transaction reconciliation
processPurchaseVsSubscriptionTransaction ::
  ( BeamFlow m r,
    EsqDBFlow m r,
    MonadFlow m,
    CacheFlow m r
  ) =>
  DSP.SubscriptionPurchase ->
  UTCTime ->
  m ReconEntry.ReconciliationEntry
processPurchaseVsSubscriptionTransaction subscription now = do
  entitled <- pure subscription.planRideCredit
  consumed <- calculateSubscriptionConsumedAmount subscription
  plan <- resolvePlanForSubscriptionPurchase subscription
  creditEntries <- QLedger.findByReference subscriptionCreditReferenceType subscription.id.getId
  let hasCreditEntry =
        any ((== LedgerEntry.SETTLED) . (.status)) creditEntries
      mismatchReason
        | not hasCreditEntry = Just DomainRecon.reasonMissingSubscriptionTransaction
        | consumed > entitled = Just DomainRecon.reasonOverSubscriptionConsumption
        | consumed < entitled = Just DomainRecon.reasonPartialSubscriptionConsumption
        | otherwise = Nothing
  entryId <- generateGUID
  let inp =
        (DomainRecon.mkDefaultReconEntryInput ReconEntry.SUBSCRIPTION_PURCHASE_VS_SUBSCRIPTION_TRANSACTION)
          { DomainRecon.bookingId = Just subscription.id.getId,
            DomainRecon.dcoId = Just subscription.ownerId,
            DomainRecon.expected = entitled,
            DomainRecon.actual = consumed,
            DomainRecon.reason = mismatchReason,
            DomainRecon.component = Just ReconEntry.SUBSCRIPTION_PURCHASE,
            DomainRecon.sourceId = Just subscription.id.getId,
            DomainRecon.transactionDate = Just subscription.purchaseTimestamp,
            DomainRecon.purchaseStatus = Just $ show subscription.status,
            DomainRecon.planName = (.name) <$> plan,
            DomainRecon.merchantId = Just subscription.merchantId.getId,
            DomainRecon.merchantOperatingCityId = Just subscription.merchantOperatingCityId.getId
          }
      entry = DomainRecon.mkReconEntry inp now entryId
  pure $
    if not hasCreditEntry
      then
        entry
          { ReconEntry.reconStatus = ReconEntry.MISSING_IN_TARGET
          }
      else entry

resolvePlanForSubscriptionPurchase ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  DSP.SubscriptionPurchase ->
  m (Maybe DPlan.Plan)
resolvePlanForSubscriptionPurchase subscription =
  CQPlan.findByIdAndPaymentModeWithServiceName subscription.planId DPlan.MANUAL subscription.serviceName
    >>= \case
      Just plan -> pure (Just plan)
      Nothing -> QPlan.findByPrimaryKey subscription.planId

-- Helper: ReconSummary and ReconEntry both define ReconciliationStatus; convert for entry records.
toReconEntryStatus :: ReconSummary.ReconciliationStatus -> ReconEntry.ReconciliationStatus
toReconEntryStatus ReconSummary.MATCHED = ReconEntry.MATCHED
toReconEntryStatus ReconSummary.HIGHER_IN_TARGET = ReconEntry.HIGHER_IN_TARGET
toReconEntryStatus ReconSummary.LOWER_IN_TARGET = ReconEntry.LOWER_IN_TARGET
toReconEntryStatus ReconSummary.MISSING_IN_TARGET = ReconEntry.MISSING_IN_TARGET
toReconEntryStatus ReconSummary.MISSING_IN_SOURCE = ReconEntry.MISSING_IN_SOURCE

-- Convert back for DB updates (Booking / SubscriptionPurchase store ReconSummary.ReconciliationStatus).
toReconSummaryStatus :: ReconEntry.ReconciliationStatus -> ReconSummary.ReconciliationStatus
toReconSummaryStatus ReconEntry.MATCHED = ReconSummary.MATCHED
toReconSummaryStatus ReconEntry.HIGHER_IN_TARGET = ReconSummary.HIGHER_IN_TARGET
toReconSummaryStatus ReconEntry.LOWER_IN_TARGET = ReconSummary.LOWER_IN_TARGET
toReconSummaryStatus ReconEntry.MISSING_IN_TARGET = ReconSummary.MISSING_IN_TARGET
toReconSummaryStatus ReconEntry.MISSING_IN_SOURCE = ReconSummary.MISSING_IN_SOURCE

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

scheduledPayoutToPayoutRequest :: ScheduledPayout.ScheduledPayout -> PayoutRequest.PayoutRequest
scheduledPayoutToPayoutRequest sp =
  PayoutRequest.PayoutRequest
    { id = cast sp.id,
      entityName = Nothing,
      entityId = sp.rideId,
      entityRefId = Just sp.bookingId,
      beneficiaryId = sp.driverId,
      amount = sp.amount,
      status = castScheduledStatus sp.status,
      retryCount = sp.retryCount,
      failureReason = sp.failureReason,
      payoutTransactionId = sp.payoutTransactionId,
      cashMarkedById = (.getId) <$> sp.markCashPaidBy,
      cashMarkedByName = Nothing,
      cashMarkedAt = Nothing,
      expectedCreditTime = sp.expectedCreditTime,
      scheduledAt = sp.expectedCreditTime,
      customerVpa = Nothing,
      customerPhone = Nothing,
      customerEmail = Nothing,
      customerName = Nothing,
      remark = Nothing,
      orderType = Nothing,
      city = Nothing,
      merchantId = maybe "" (.getId) sp.merchantId,
      merchantOperatingCityId = maybe "" (.getId) sp.merchantOperatingCityId,
      payoutFee = Nothing,
      payoutType = Nothing,
      coverageFrom = Nothing,
      coverageTo = Nothing,
      createdAt = sp.createdAt,
      updatedAt = sp.updatedAt,
      ledgerEntryIds = Nothing
    }

castScheduledStatus :: ScheduledPayout.ScheduledPayoutStatus -> PayoutRequest.PayoutRequestStatus
castScheduledStatus = \case
  ScheduledPayout.INITIATED -> PayoutRequest.INITIATED
  ScheduledPayout.PROCESSING -> PayoutRequest.PROCESSING
  ScheduledPayout.CREDITED -> PayoutRequest.CREDITED
  ScheduledPayout.AUTO_PAY_FAILED -> PayoutRequest.AUTO_PAY_FAILED
  ScheduledPayout.RETRYING -> PayoutRequest.RETRYING
  ScheduledPayout.FAILED -> PayoutRequest.FAILED
  ScheduledPayout.CANCELLED -> PayoutRequest.CANCELLED
  ScheduledPayout.CASH_PAID -> PayoutRequest.CASH_PAID
  ScheduledPayout.CASH_PENDING -> PayoutRequest.CASH_PENDING
