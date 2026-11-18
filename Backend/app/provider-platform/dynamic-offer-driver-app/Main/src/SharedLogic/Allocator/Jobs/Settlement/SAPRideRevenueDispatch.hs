module SharedLogic.Allocator.Jobs.Settlement.SAPRideRevenueDispatch
  ( runSAPRideRevenueDispatchJob,
    -- JV event labels → sap_journal_entry.description (phase-1). May become a dedicated field later.
    onlineRideRevRecLabel,
    offlineCashRideLabel,
    buyerAppSettlementLabel,
    driverEarningAccrualLabel,
    payoutInitiatedLabel,
    payoutSettledAtBankLabel,
    payoutFailedLabel,
    payoutReversalLabel,
    tdsReimbursementLabel,
    subscriptionRideRevenueLabel,
    subscriptionExpiryRevenueLabel,
  )
where

import qualified Data.Map.Strict as M
import qualified Domain.Types.BapMetadata as DBapMetadata
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Kernel.Beam.Lib.UtilsTH (HasSchemaName)
import Kernel.External.Encryption ()
import qualified Kernel.External.SAP.Config as SAPConfig
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common
import qualified Lib.Finance.Core.Types as Finance
import qualified Lib.Finance.Domain.Types.JournalEntryTransaction as JET
import qualified Lib.Finance.Domain.Types.SapJournalEntry as SJE
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import Lib.Scheduler
import Lib.Scheduler.JobStorageType.DB.Table (SchedulerJobT)
import qualified Lib.Scheduler.JobStorageType.SchedulerType as JC
import SharedLogic.Allocator (AllocatorJobType (..), SAPRideRevenueDispatchJobData (..))
import SharedLogic.Allocator.Jobs.Settlement.RideRevenueTotals
import SharedLogic.Allocator.Jobs.Settlement.SAPDispatchCommon
import Storage.Beam.SchedulerJob ()
import qualified Storage.CachedQueries.BapMetadata as CQBapMetadata
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Tools.Error

-- ---------------------------------------------------------------------------
-- JV event labels (written to sap_journal_entry.description; shared with SAP GET drill-down)
-- ---------------------------------------------------------------------------

onlineRideRevRecLabel :: Text
onlineRideRevRecLabel = "Buyer App Online Ride Completed"

offlineCashRideLabel :: Text
offlineCashRideLabel = "Cash Ride Driver Earning Tax Recovery"

buyerAppSettlementLabel :: Text
buyerAppSettlementLabel = "Online Ride Buyer App Settlement"

driverEarningAccrualLabel :: Text
driverEarningAccrualLabel = "Online Ride Driver Earning"

payoutInitiatedLabel :: Text
payoutInitiatedLabel = "Payout request by Driver"

payoutSettledAtBankLabel :: Text
payoutSettledAtBankLabel = "Payout Settlement to FO"

payoutFailedLabel :: Text
payoutFailedLabel = "Payout failed at processing stage"

payoutReversalLabel :: Text
payoutReversalLabel = "Payout Reversal in Bank - Driver"

tdsReimbursementLabel :: Text
tdsReimbursementLabel = "TDS Reimbursement Payable"

subscriptionRideRevenueLabel :: Text
subscriptionRideRevenueLabel = "Subscription Ride Revenue Recognised"

subscriptionExpiryRevenueLabel :: Text
subscriptionExpiryRevenueLabel = "Subscription Expiry Revenue Recognised"

-- ---------------------------------------------------------------------------
-- Ride Revenue Dispatch Job
-- ---------------------------------------------------------------------------

runSAPRideRevenueDispatchJob ::
  (SAPJobConstraints m r c) =>
  Job 'SAPRideRevenueDispatch ->
  m ExecutionResult
runSAPRideRevenueDispatchJob Job {id, jobInfo} = do
  let jobData = jobInfo.jobData
  runSAPDispatchShell
    id.getId
    SAPDispatchShellCfg
      { lockKeyPrefix = "SAPRideRevenueDispatch",
        idempotencyJobType = "RevenueRecognition",
        jobLabel = "ride revenue"
      }
    (mkSAPDispatchJobParams jobData)
    scheduleNextRideRevenueJob
    ( \sapCfg token params -> do
        totals <- fetchRideRevenueTotals params.merchantOperatingCityId params.startTime params.endTime
        dispatchRideRevenue sapCfg token params totals
    )

-- ---------------------------------------------------------------------------
-- Dispatch all matrix events (zero amounts skip — totals may still be stubbed)
-- ---------------------------------------------------------------------------

dispatchRideRevenue ::
  ( BeamFlow m r,
    EncFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    CoreMetrics m,
    Finance.HasActorInfo m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  SAPConfig.SAPServiceConfig ->
  Text ->
  SAPDispatchJobParams ->
  RideRevenueTotals ->
  m Bool
dispatchRideRevenue sapCfg token params totals = do
  merchantOperatingCity <- CQMOC.findById params.merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityNotFound params.merchantOperatingCityId.getId)
  let currency = merchantOperatingCity.currency
      allBapIds = M.keys totals.onlineRideRevRec <> M.keys totals.buyerAppSettlement <> M.keys totals.driverEarningAccrual
  bapNameMap <-
    M.fromList . catMaybes
      <$> forM
        allBapIds
        ( \bapId -> do
            mbMeta <- CQBapMetadata.findBySubscriberIdAndDomain (Id bapId) Context.MOBILITY
            pure $ (bapId,) . (.name) <$> mbMeta
        )
  results <-
    forM (mkJVSpecs params.merchantId params.merchantOperatingCityId currency totals bapNameMap) $
      postJV sapCfg token params currency
  pure $ and results

-- | One JVSpec per matrix event, in dispatch order.
mkJVSpecs ::
  (BeamFlow m r, Finance.HasActorInfo m r) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Currency ->
  RideRevenueTotals ->
  M.Map Text Text ->
  [JVSpec m]
mkJVSpecs mId mocid currency totals bapNameMap =
  concat
    [ -- 1/3. Ride fare rev-rec (online or offline)
      -- Online: Dr BUYER_APP_RECEIVABLE / Cr RIDE_FARE_REVENUE + GST (per bapId)
      -- Offline: Dr DRIVER_BALANCE / Cr RIDE_FARE_REVENUE + GST
      mkOnlineRideRevRecSpecsByBapId mId mocid currency totals.onlineRideRevRec bapNameMap,
      -- 2. Buyer-app settlement: Dr BANK / Cr BUYER_APP_RECEIVABLE (per bapId)
      mkBuyerAppSettlementSpecsByBapId mId mocid currency totals.buyerAppSettlement bapNameMap,
      [mkSpec offlineCashRideLabel SJE.RideFareRevenue JET.Booking (rideFareRevRecLegs driverBalanceAcct offlineTotals) offlineTotals.txnCount offlineRows noHeaderAttrs],
      -- 4. Driver earning accrual: Dr RIDE_FARE_REVENUE / Cr DRIVER_BALANCE (per bapId)
      mkDriverEarningAccrualSpecsByBapId mId mocid currency totals.driverEarningAccrual bapNameMap,
      -- 5. Payout lifecycle JVs (from payout_lifecycle_event)
      -- 5a. Payout initiated: Dr DRIVER_BALANCE / Cr PAYOUT_CLEARING
      [mkSpec payoutInitiatedLabel SJE.DriverPayout JET.Payout [JVLeg driverBalanceAcct Debit piTotals.totalAmount, JVLeg payoutClearingAcct Credit piTotals.totalAmount] piTotals.txnCount piRows noHeaderAttrs],
      -- 5b. Payout settled at bank: Dr PAYOUT_CLEARING / Cr BANK
      [mkSpec payoutSettledAtBankLabel SJE.DriverPayout JET.Payout [JVLeg payoutClearingAcct Debit psTotals.totalAmount, JVLeg bankAcct Credit psTotals.totalAmount] psTotals.txnCount psRows noHeaderAttrs],
      -- 5c. Payout failed: Dr PAYOUT_CLEARING / Cr DRIVER_BALANCE
      [mkSpec payoutFailedLabel SJE.DriverPayout JET.Payout [JVLeg payoutClearingAcct Debit pfTotals.totalAmount, JVLeg driverBalanceAcct Credit pfTotals.totalAmount] pfTotals.txnCount pfRows noHeaderAttrs],
      -- 5d. Payout reversal (Option B): Dr BANK / Cr DRIVER_BALANCE
      [mkSpec payoutReversalLabel SJE.DriverPayout JET.Payout [JVLeg bankAcct Debit prTotals.totalAmount, JVLeg driverBalanceAcct Credit prTotals.totalAmount] prTotals.txnCount prRows noHeaderAttrs],
      -- 6. TDS reimbursement: Dr TDS_RECEIVABLE / Cr DRIVER_BALANCE
      --    (TDS deduction is now handled via ledger adjustments config — TdsThresholdDeduction)
      [mkSpec tdsReimbursementLabel SJE.TDS JET.TdsReimbursementRequest [JVLeg tdsReceivableAcct Debit tdsTotals.reimbursementAmount, JVLeg driverBalanceAcct Credit tdsTotals.reimbursementAmount] tdsTotals.reimbursementCount reimbursementRows noHeaderAttrs],
      -- 6b. Ledger adjustments (config-driven, one JV per entry in ledgerAdjustmentJVConfigs)
      mkLedgerAdjustmentSpecs mId mocid currency totals.ledgerAdjustments,
      -- 7. Subscription revenue recognised: Dr DEFERRED_REVENUE / Cr SUBSCRIPTION_REVENUE
      --    Ride vs expiry use the same legs, different description labels.
      [mkSpec subscriptionRideRevenueLabel SJE.RevenueRecognition JET.SubscriptionPurchase (subscriptionRevenueLegs rideSubTotals) rideSubTotals.txnCount rideSubRows noHeaderAttrs],
      [mkSpec subscriptionExpiryRevenueLabel SJE.RevenueRecognition JET.SubscriptionPurchase (subscriptionRevenueLegs expirySubTotals) expirySubTotals.txnCount expirySubRows noHeaderAttrs]
    ]
  where
    (offlineTotals, offlineRows) = totals.offlineCashRide
    (piTotals, piRows) = totals.payoutInitiated
    (psTotals, psRows) = totals.payoutSettled
    (pfTotals, pfRows) = totals.payoutFailed
    (prTotals, prRows) = totals.payoutReversed
    (tdsTotals, _deductionRows, reimbursementRows) = totals.tds
    (rideSubTotals, rideSubRows) = totals.subscriptionRideRevenue
    (expirySubTotals, expirySubRows) = totals.subscriptionExpiryRevenue
    mkSpec label txnType refType legs txnCount rows attrs =
      JVSpec
        { label,
          legs,
          transactionType = txnType,
          txnCount,
          headerAttrs = attrs,
          bapId = Nothing,
          bapName = Nothing,
          saveRows = \sapEntryId sapBatchId -> saveRevenueRecognitionTransactions mId mocid sapEntryId sapBatchId label refType txnType currency rows
        }

rideFareRevRecLegs :: Text -> RideFareRevRecTotals -> [JVLeg]
rideFareRevRecLegs debitAcct totals =
  [ JVLeg debitAcct Debit totals.grossAmount,
    JVLeg rideFareRevenueAcct Credit totals.netAmount,
    JVLeg cgstPayableAcct Credit totals.cgst,
    JVLeg sgstPayableAcct Credit totals.sgst,
    JVLeg igstPayableAcct Credit totals.igst
  ]

subscriptionRevenueLegs :: SubscriptionRevenueTotals -> [JVLeg]
subscriptionRevenueLegs totals =
  [ JVLeg deferredRevenueAcct Debit totals.recognizedAmount,
    JVLeg subscriptionRevenueAcct Credit totals.recognizedAmount
  ]

mkBapHeaderAttrs :: Text -> M.Map Text Text -> JVHeaderAttrs
mkBapHeaderAttrs bapId bapNameMap =
  JVHeaderAttrs
    { attrName1 = Just "Buyer Merchant Id",
      attrValue1 = Just bapId,
      attrName2 = Just "Buyer Merchant Name",
      attrValue2 = M.lookup bapId bapNameMap,
      attrName3 = Nothing,
      attrValue3 = Nothing,
      attrName4 = Nothing,
      attrValue4 = Nothing,
      attrName5 = Nothing,
      attrValue5 = Nothing
    }

-- | Expand online ride rev-rec into one JVSpec per bapId.
mkOnlineRideRevRecSpecsByBapId ::
  (BeamFlow m r, Finance.HasActorInfo m r) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Currency ->
  M.Map Text (RideFareRevRecTotals, [RideRevenueTransactionRow]) ->
  M.Map Text Text ->
  [JVSpec m]
mkOnlineRideRevRecSpecsByBapId mId mocid currency onlineMap bapNameMap =
  map
    ( \(bId, (t, rows)) ->
        JVSpec
          { label = onlineRideRevRecLabel,
            legs = rideFareRevRecLegs buyerAppReceivableAcct t,
            transactionType = SJE.RideFareRevenue,
            txnCount = t.txnCount,
            headerAttrs = mkBapHeaderAttrs bId bapNameMap,
            bapId = Just bId,
            bapName = M.lookup bId bapNameMap,
            saveRows = \sapEntryId sapBatchId -> saveRevenueRecognitionTransactions mId mocid sapEntryId sapBatchId onlineRideRevRecLabel JET.Booking SJE.RideFareRevenue currency rows
          }
    )
    (M.toList onlineMap)

-- | Expand buyer-app settlement into one JVSpec per bapId.
mkBuyerAppSettlementSpecsByBapId ::
  (BeamFlow m r, Finance.HasActorInfo m r) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Currency ->
  M.Map Text (BuyerAppSettlementTotals, [RideRevenueTransactionRow]) ->
  M.Map Text Text ->
  [JVSpec m]
mkBuyerAppSettlementSpecsByBapId mId mocid currency settlementMap bapNameMap =
  map
    ( \(bId, (t, rows)) ->
        JVSpec
          { label = buyerAppSettlementLabel,
            legs = [JVLeg bankAcct Debit t.settledAmount, JVLeg buyerAppReceivableAcct Credit t.settledAmount],
            transactionType = SJE.BuyerAppSettlement,
            txnCount = t.txnCount,
            headerAttrs = mkBapHeaderAttrs bId bapNameMap,
            bapId = Just bId,
            bapName = M.lookup bId bapNameMap,
            saveRows = \sapEntryId sapBatchId -> saveRevenueRecognitionTransactions mId mocid sapEntryId sapBatchId buyerAppSettlementLabel JET.Booking SJE.BuyerAppSettlement currency rows
          }
    )
    (M.toList settlementMap)

-- | Expand driver earning accrual into one JVSpec per bapId.
mkDriverEarningAccrualSpecsByBapId ::
  (BeamFlow m r, Finance.HasActorInfo m r) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Currency ->
  M.Map Text (DriverEarningAccrualTotals, [RideRevenueTransactionRow]) ->
  M.Map Text Text ->
  [JVSpec m]
mkDriverEarningAccrualSpecsByBapId mId mocid currency accrualMap bapNameMap =
  map
    ( \(bId, (t, rows)) ->
        JVSpec
          { label = driverEarningAccrualLabel,
            legs = [JVLeg rideFareRevenueAcct Debit t.accrualAmount, JVLeg driverBalanceAcct Credit t.accrualAmount],
            transactionType = SJE.DriverEarningAccrual,
            txnCount = t.txnCount,
            headerAttrs = mkBapHeaderAttrs bId bapNameMap,
            bapId = Just bId,
            bapName = M.lookup bId bapNameMap,
            saveRows = \sapEntryId sapBatchId -> saveRevenueRecognitionTransactions mId mocid sapEntryId sapBatchId driverEarningAccrualLabel JET.Booking SJE.DriverEarningAccrual currency rows
          }
    )
    (M.toList accrualMap)

-- | Config-driven JVSpecs from ledger adjustments. One JV per entry in the config list.
mkLedgerAdjustmentSpecs ::
  (BeamFlow m r, Finance.HasActorInfo m r) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Currency ->
  [(LedgerAdjustmentJVConfig, (PayoutEventTotals, [RideRevenueTransactionRow]))] ->
  [JVSpec m]
mkLedgerAdjustmentSpecs mId mocid currency =
  map $ \(cfg, (t, rows)) ->
    JVSpec
      { label = cfg.label,
        legs = [JVLeg cfg.debitAcct Debit t.totalAmount, JVLeg cfg.creditAcct Credit t.totalAmount],
        transactionType = cfg.transactionType,
        txnCount = t.txnCount,
        headerAttrs = noHeaderAttrs,
        bapId = Nothing,
        bapName = Nothing,
        saveRows = \sapEntryId sapBatchId -> saveRevenueRecognitionTransactions mId mocid sapEntryId sapBatchId cfg.label cfg.referenceJetType cfg.transactionType currency rows
      }

scheduleNextRideRevenueJob ::
  ( BeamFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    JobCreatorEnv r,
    HasSchemaName SchedulerJobT,
    HasField "schedulerType" r SchedulerType
  ) =>
  NextSAPDispatchSchedule ->
  m ()
scheduleNextRideRevenueJob NextSAPDispatchSchedule {scheduleAfter, minScheduleTime, maxScheduleTime, jobParams} = do
  JC.createJobInWithCheck @_ @'SAPRideRevenueDispatch (Just jobParams.merchantId) (Just jobParams.merchantOperatingCityId) scheduleAfter minScheduleTime maxScheduleTime "SAPRideRevenueDispatch" (Just 1) (mkSAPRideRevenueDispatchJobData jobParams)

-- ---------------------------------------------------------------------------
-- Journal Entry Transaction persistence (individual source transactions)
-- ---------------------------------------------------------------------------

saveRevenueRecognitionTransactions ::
  (BeamFlow m r, Finance.HasActorInfo m r) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Id SJE.SapJournalEntry ->
  Text ->
  Text ->
  JET.ReferenceType ->
  SJE.TransactionType ->
  Currency ->
  [RideRevenueTransactionRow] ->
  m ()
saveRevenueRecognitionTransactions mId mocId sapEntryId batchId label refType txnType currency =
  saveJournalEntryTransactions mId mocId sapEntryId batchId currency $ \row ->
    JournalTxnRowFields
      { debitAmount = row.amount,
        creditAmount = row.amount,
        description = label,
        referenceId = Just row.referenceId,
        referenceType = Just refType,
        transactionType = txnType,
        status = row.txnStatus
      }

-- ---------------------------------------------------------------------------
-- Helper functions
-- ---------------------------------------------------------------------------

mkSAPDispatchJobParams :: SAPRideRevenueDispatchJobData -> SAPDispatchJobParams
mkSAPDispatchJobParams SAPRideRevenueDispatchJobData {..} = SAPDispatchJobParams {..}

mkSAPRideRevenueDispatchJobData :: SAPDispatchJobParams -> SAPRideRevenueDispatchJobData
mkSAPRideRevenueDispatchJobData SAPDispatchJobParams {..} = SAPRideRevenueDispatchJobData {..}
