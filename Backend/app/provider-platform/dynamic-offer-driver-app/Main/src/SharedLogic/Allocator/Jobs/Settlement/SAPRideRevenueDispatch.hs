module SharedLogic.Allocator.Jobs.Settlement.SAPRideRevenueDispatch
  ( runSAPRideRevenueDispatchJob,
    -- JV event labels → sap_journal_entry.description (phase-1). May become a dedicated field later.
    onlineRideRevRecLabel,
    offlineCashRideLabel,
    buyerAppSettlementLabel,
    driverEarningAccrualLabel,
    payoutToClearingLabel,
    payoutClearingToBankLabel,
    tdsDeductionLabel,
    tdsReimbursementLabel,
    subscriptionRideRevenueLabel,
    subscriptionExpiryRevenueLabel,
  )
where

import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Kernel.Beam.Lib.UtilsTH (HasSchemaName)
import Kernel.External.Encryption ()
import qualified Kernel.External.SAP.Config as SAPConfig
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
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
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Tools.Error

-- ---------------------------------------------------------------------------
-- JV event labels (written to sap_journal_entry.description; shared with SAP GET drill-down)
-- ---------------------------------------------------------------------------

onlineRideRevRecLabel :: Text
onlineRideRevRecLabel = "OnlineRideRevRec"

offlineCashRideLabel :: Text
offlineCashRideLabel = "OfflineCashRide"

buyerAppSettlementLabel :: Text
buyerAppSettlementLabel = "BuyerAppSettlement"

driverEarningAccrualLabel :: Text
driverEarningAccrualLabel = "DriverEarningAccrual"

payoutToClearingLabel :: Text
payoutToClearingLabel = "PayoutToClearing"

payoutClearingToBankLabel :: Text
payoutClearingToBankLabel = "PayoutClearingToBank"

tdsDeductionLabel :: Text
tdsDeductionLabel = "TdsDeduction"

tdsReimbursementLabel :: Text
tdsReimbursementLabel = "TdsReimbursement"

subscriptionRideRevenueLabel :: Text
subscriptionRideRevenueLabel = "SubscriptionRideRevenue"

subscriptionExpiryRevenueLabel :: Text
subscriptionExpiryRevenueLabel = "SubscriptionExpiryRevenue"

-- ---------------------------------------------------------------------------
-- AccountMapping keys (must match MerchantServiceConfig SAP_Journal seed / 0011 migration)
-- ---------------------------------------------------------------------------

buyerAppReceivableAcct :: Text
buyerAppReceivableAcct = "BUYER_APP_RECEIVABLE A/C"

rideFareRevenueAcct :: Text
rideFareRevenueAcct = "RIDE_FARE_REVENUE A/C"

driverBalanceAcct :: Text
driverBalanceAcct = "DRIVER_BALANCE A/C"

payoutClearingAcct :: Text
payoutClearingAcct = "PAYOUT_CLEARING A/C"

tdsPayableAcct :: Text
tdsPayableAcct = "TDS_PAYABLE A/C"

tdsReceivableAcct :: Text
tdsReceivableAcct = "TDS_RECEIVABLE A/C"

bankAcct :: Text
bankAcct = "BANK A/C"

cgstPayableAcct :: Text
cgstPayableAcct = "CGST_PAYABLE A/C"

sgstPayableAcct :: Text
sgstPayableAcct = "SGST_PAYABLE A/C"

igstPayableAcct :: Text
igstPayableAcct = "IGST_PAYABLE A/C"

deferredRevenueAcct :: Text
deferredRevenueAcct = "DEFERRED_REVENUE A/C"

subscriptionRevenueAcct :: Text
subscriptionRevenueAcct = "SUBSCRIPTION_REVENUE A/C"

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
  results <-
    forM (mkJVSpecs params.merchantId params.merchantOperatingCityId currency totals) $
      postJV sapCfg token params currency SJE.RevenueRecognition
  pure $ and results

-- | One JVSpec per matrix event, in dispatch order.
mkJVSpecs ::
  (BeamFlow m r, Finance.HasActorInfo m r) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Currency ->
  RideRevenueTotals ->
  [JVSpec m]
mkJVSpecs mId mocid currency totals =
  [ -- 1/3. Ride fare rev-rec (online or offline)
    -- Online: Dr BUYER_APP_RECEIVABLE / Cr RIDE_FARE_REVENUE + GST
    -- Online: Dr DRIVER_BALANCE / Cr RIDE_FARE_REVENUE + GST
    mkSpec onlineRideRevRecLabel JET.Booking (rideFareRevRecLegs buyerAppReceivableAcct onlineTotals) onlineTotals.txnCount onlineRows,
    -- 2. Buyer-app settlement: Dr BANK / Cr BUYER_APP_RECEIVABLE
    mkSpec buyerAppSettlementLabel JET.Booking [JVLeg bankAcct Debit settleTotals.settledAmount, JVLeg buyerAppReceivableAcct Credit settleTotals.settledAmount] settleTotals.txnCount settleRows,
    mkSpec offlineCashRideLabel JET.Booking (rideFareRevRecLegs driverBalanceAcct offlineTotals) offlineTotals.txnCount offlineRows,
    -- 4. Driver earning accrual: Dr RIDE_FARE_REVENUE / Cr DRIVER_BALANCE
    mkSpec driverEarningAccrualLabel JET.Booking [JVLeg rideFareRevenueAcct Debit accrualTotals.accrualAmount, JVLeg driverBalanceAcct Credit accrualTotals.accrualAmount] accrualTotals.txnCount accrualRows,
    -- 5. Payout: two balanced JVs. Phase-1 both use the same WalletPayout total
    --    (clearing is a same-day wash). Intended sources:
    --      PayoutToClearing      = ledger WalletPayout (driver liability debit on SUCCESS)
    --      PayoutClearingToBank  = pg_payout_settlement_report (PG/bank file; WS4 ingest, idealy separate scheduler job should be created)
    mkSpec payoutToClearingLabel JET.Payout [JVLeg driverBalanceAcct Debit payoutTotals.payoutAmount, JVLeg payoutClearingAcct Credit payoutTotals.payoutAmount] payoutTotals.txnCount payoutRows,
    -- Same amount as JV1 until WS4 wires pg_payout_settlement_report.
    -- JV2: Dr PAYOUT_CLEARING / Cr BANK
    mkSpec payoutClearingToBankLabel JET.Payout [JVLeg payoutClearingAcct Debit payoutTotals.payoutAmount, JVLeg bankAcct Credit payoutTotals.payoutAmount] payoutTotals.txnCount payoutRows,
    -- 6. TDS: deduction Dr DRIVER_BALANCE / Cr TDS_PAYABLE; reimbursement Dr TDS_RECEIVABLE / Cr DRIVER_BALANCE
    mkSpec tdsDeductionLabel JET.Booking [JVLeg driverBalanceAcct Debit tdsTotals.deductionAmount, JVLeg tdsPayableAcct Credit tdsTotals.deductionAmount] tdsTotals.deductionCount deductionRows,
    mkSpec tdsReimbursementLabel JET.TdsReimbursementRequest [JVLeg tdsReceivableAcct Debit tdsTotals.reimbursementAmount, JVLeg driverBalanceAcct Credit tdsTotals.reimbursementAmount] tdsTotals.reimbursementCount reimbursementRows,
    -- 7. Subscription revenue recognised: Dr DEFERRED_REVENUE / Cr SUBSCRIPTION_REVENUE
    --    Ride vs expiry use the same legs, different description labels.
    mkSpec subscriptionRideRevenueLabel JET.SubscriptionPurchase (subscriptionRevenueLegs rideSubTotals) rideSubTotals.txnCount rideSubRows,
    mkSpec subscriptionExpiryRevenueLabel JET.SubscriptionPurchase (subscriptionRevenueLegs expirySubTotals) expirySubTotals.txnCount expirySubRows
  ]
  where
    (onlineTotals, onlineRows) = totals.onlineRideRevRec
    (settleTotals, settleRows) = totals.buyerAppSettlement
    (offlineTotals, offlineRows) = totals.offlineCashRide
    (accrualTotals, accrualRows) = totals.driverEarningAccrual
    (payoutTotals, payoutRows) = totals.payout
    (tdsTotals, deductionRows, reimbursementRows) = totals.tds
    (rideSubTotals, rideSubRows) = totals.subscriptionRideRevenue
    (expirySubTotals, expirySubRows) = totals.subscriptionExpiryRevenue
    mkSpec label refType legs txnCount rows =
      JVSpec
        { label,
          legs,
          txnCount,
          saveRows = \sapEntryId sapBatchId -> saveRevenueRecognitionTransactions mId mocid sapEntryId sapBatchId label refType currency rows
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
  Currency ->
  [RevenueRecognitionTransactionRow] ->
  m ()
saveRevenueRecognitionTransactions mId mocId sapEntryId batchId label refType currency =
  saveJournalEntryTransactions mId mocId sapEntryId batchId currency $ \row ->
    JournalTxnRowFields
      { debitAmount = row.amount,
        creditAmount = row.amount,
        description = label,
        referenceId = Just row.referenceId,
        referenceType = Just refType,
        transactionType = SJE.RevenueRecognition,
        status = row.txnStatus
      }

-- ---------------------------------------------------------------------------
-- Helper functions
-- ---------------------------------------------------------------------------

mkSAPDispatchJobParams :: SAPRideRevenueDispatchJobData -> SAPDispatchJobParams
mkSAPDispatchJobParams SAPRideRevenueDispatchJobData {..} = SAPDispatchJobParams {..}

mkSAPRideRevenueDispatchJobData :: SAPDispatchJobParams -> SAPRideRevenueDispatchJobData
mkSAPRideRevenueDispatchJobData SAPDispatchJobParams {..} = SAPRideRevenueDispatchJobData {..}
