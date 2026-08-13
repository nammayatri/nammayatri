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
import Kernel.External.SAP.Types (SAPJournalItem (..))
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common
import qualified Lib.Finance.Core.Types as Finance
import qualified Lib.Finance.Domain.Types.JournalEntryTransaction as JET
import qualified Lib.Finance.Domain.Types.SapJournalEntry as SJE
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Finance.Storage.Queries.JournalEntryTransaction as QJETExtra
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
  onlineOk <-
    uncurry
      (dispatchRideFareRevRec sapCfg token params onlineRideRevRecLabel buyerAppReceivableAcct currency)
      totals.onlineRideRevRec
  settleOk <- uncurry (dispatchBuyerAppSettlement sapCfg token params currency) totals.buyerAppSettlement
  offlineOk <-
    uncurry
      (dispatchRideFareRevRec sapCfg token params offlineCashRideLabel driverBalanceAcct currency)
      totals.offlineCashRide
  accrualOk <- uncurry (dispatchDriverEarningAccrual sapCfg token params currency) totals.driverEarningAccrual
  payoutOk <- uncurry (dispatchPayout sapCfg token params currency) totals.payout
  tdsOk <-
    let (tdsTotals, deductionRows, reimbursementRows) = totals.tds
     in dispatchTds sapCfg token params currency tdsTotals deductionRows reimbursementRows
  rideSubOk <- uncurry (dispatchSubscriptionRevenue sapCfg token params subscriptionRideRevenueLabel currency) totals.subscriptionRideRevenue
  expirySubOk <- uncurry (dispatchSubscriptionRevenue sapCfg token params subscriptionExpiryRevenueLabel currency) totals.subscriptionExpiryRevenue
  pure $ onlineOk && settleOk && offlineOk && accrualOk && payoutOk && tdsOk && rideSubOk && expirySubOk

postRevenueRecognitionJV ::
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
  Text ->
  Int ->
  [SAPJournalItem] ->
  Currency ->
  [RevenueRecognitionTransactionRow] ->
  m Bool
postRevenueRecognitionJV sapCfg token params label txnCount items currency rows = do
  let SAPDispatchJobParams
        { merchantId = mId,
          merchantOperatingCityId = mocid,
          maxApiRetries = maxRetries,
          startTime = fromTime,
          endTime = toTime
        } = params
  let filtered = filterZeroItems items
  if null filtered
    then do
      logInfo $ "No non-zero items for " <> label <> ", skipping"
      pure True
    else do
      logInfo $ "Dispatching " <> label <> " to SAP, txnCount=" <> show txnCount
      req <- buildJournalRequestFromItems sapCfg label fromTime filtered
      logInfo $ "SAP journal entry request body = " <> show req
      result <- callSAPWithRetry sapCfg token req label maxRetries
      let saveTransactionAction sapEntryId sapBatchId =
            saveRevenueRecognitionTransactions mId mocid sapEntryId sapBatchId label currency rows
      handleSAPResponse label req result SJE.RevenueRecognition txnCount mId mocid fromTime toTime currency saveTransactionAction

-- 1/3. Ride fare rev-rec (online or offline)
-- Online: Dr BUYER_APP_RECEIVABLE / Cr RIDE_FARE_REVENUE + GST
-- Online: Dr DRIVER_BALANCE / Cr RIDE_FARE_REVENUE + GST
dispatchRideFareRevRec ::
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
  Text ->
  Text ->
  Currency ->
  RideFareRevRecTotals ->
  [RevenueRecognitionTransactionRow] ->
  m Bool
dispatchRideFareRevRec _sapCfg _token _params label _debitAcct _currency totals _rows
  | totals.grossAmount == 0 && totals.netAmount == 0 = do
    logInfo $ "No " <> label <> " totals, skipping"
    pure True
dispatchRideFareRevRec sapCfg token params label debitAcct currency totals rows = do
  let acctMap = sapCfg.accountMapping
  bId <- getNextBatchId
  items <-
    sequence
      [ mkItem bId "1" debitAcct acctMap Debit totals.grossAmount currency,
        mkItem bId "2" rideFareRevenueAcct acctMap Credit totals.netAmount currency,
        mkItem bId "3" cgstPayableAcct acctMap Credit totals.cgst currency,
        mkItem bId "4" sgstPayableAcct acctMap Credit totals.sgst currency,
        mkItem bId "5" igstPayableAcct acctMap Credit totals.igst currency
      ]
  let debit = totals.grossAmount
      credit = totals.netAmount + totals.cgst + totals.sgst + totals.igst
  assertDebitEqualsCredit label bId debit credit
  postRevenueRecognitionJV sapCfg token params label totals.txnCount items currency rows

-- 2. Buyer-app settlement: Dr BANK / Cr BUYER_APP_RECEIVABLE
dispatchBuyerAppSettlement ::
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
  Currency ->
  BuyerAppSettlementTotals ->
  [RevenueRecognitionTransactionRow] ->
  m Bool
dispatchBuyerAppSettlement _ _ _params _currency totals _rows
  | totals.settledAmount == 0 = do
    logInfo "No buyer-app settlement totals, skipping"
    pure True
dispatchBuyerAppSettlement sapCfg token params currency totals rows = do
  let acctMap = sapCfg.accountMapping
  bId <- getNextBatchId
  items <-
    sequence
      [ mkItem bId "1" bankAcct acctMap Debit totals.settledAmount currency,
        mkItem bId "2" buyerAppReceivableAcct acctMap Credit totals.settledAmount currency
      ]
  postRevenueRecognitionJV sapCfg token params buyerAppSettlementLabel totals.txnCount items currency rows

-- 4. Driver earning accrual: Dr RIDE_FARE_REVENUE / Cr DRIVER_BALANCE
dispatchDriverEarningAccrual ::
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
  Currency ->
  DriverEarningAccrualTotals ->
  [RevenueRecognitionTransactionRow] ->
  m Bool
dispatchDriverEarningAccrual _ _ _params _currency totals _rows
  | totals.accrualAmount == 0 = do
    logInfo "No driver earning accrual totals, skipping"
    pure True
dispatchDriverEarningAccrual sapCfg token params currency totals rows = do
  let acctMap = sapCfg.accountMapping
  bId <- getNextBatchId
  items <-
    sequence
      [ mkItem bId "1" rideFareRevenueAcct acctMap Debit totals.accrualAmount currency,
        mkItem bId "2" driverBalanceAcct acctMap Credit totals.accrualAmount currency
      ]
  postRevenueRecognitionJV sapCfg token params driverEarningAccrualLabel totals.txnCount items currency rows

-- 5. Payout: DRIVER_BALANCE → PAYOUT_CLEARING → BANK (two balanced JVs in one request via two headers is heavier;
--    phase-1: single multi-line JV with clearing in the middle nets Dr==Cr)
dispatchPayout ::
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
  Currency ->
  PayoutTotals ->
  [RevenueRecognitionTransactionRow] ->
  m Bool
dispatchPayout _ _ _params _currency totals _rows
  | totals.payoutAmount == 0 = do
    logInfo "No payout totals, skipping"
    pure True
dispatchPayout sapCfg token params currency totals rows = do
  let acctMap = sapCfg.accountMapping
      amount = totals.payoutAmount
  bId1 <- getNextBatchId
  items1 <-
    sequence
      [ mkItem bId1 "1" driverBalanceAcct acctMap Debit amount currency,
        mkItem bId1 "2" payoutClearingAcct acctMap Credit amount currency
      ]
  ok1 <- postRevenueRecognitionJV sapCfg token params payoutToClearingLabel totals.txnCount items1 currency rows
  -- JV2: Dr PAYOUT_CLEARING / Cr BANK
  bId2 <- getNextBatchId
  items2 <-
    sequence
      [ mkItem bId2 "1" payoutClearingAcct acctMap Debit amount currency,
        mkItem bId2 "2" bankAcct acctMap Credit amount currency
      ]
  ok2 <- postRevenueRecognitionJV sapCfg token params payoutClearingToBankLabel totals.txnCount items2 currency rows
  pure $ ok1 && ok2

-- 6. TDS: deduction Dr DRIVER_BALANCE / Cr TDS_PAYABLE; reimbursement Dr TDS_RECEIVABLE / Cr DRIVER_BALANCE
dispatchTds ::
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
  Currency ->
  TdsTotals ->
  [RevenueRecognitionTransactionRow] ->
  [RevenueRecognitionTransactionRow] ->
  m Bool
dispatchTds sapCfg token params currency totals deductionRows reimbursementRows = do
  dedOk <-
    if totals.deductionAmount == 0
      then do
        logInfo "No TDS deduction totals, skipping"
        pure True
      else do
        let acctMap = sapCfg.accountMapping
        bId <- getNextBatchId
        items <-
          sequence
            [ mkItem bId "1" driverBalanceAcct acctMap Debit totals.deductionAmount currency,
              mkItem bId "2" tdsPayableAcct acctMap Credit totals.deductionAmount currency
            ]
        postRevenueRecognitionJV sapCfg token params tdsDeductionLabel totals.deductionCount items currency deductionRows
  reimbOk <-
    if totals.reimbursementAmount == 0
      then do
        logInfo "No TDS reimbursement totals, skipping"
        pure True
      else do
        let acctMap = sapCfg.accountMapping
        bId <- getNextBatchId
        items <-
          sequence
            [ mkItem bId "1" tdsReceivableAcct acctMap Debit totals.reimbursementAmount currency,
              mkItem bId "2" driverBalanceAcct acctMap Credit totals.reimbursementAmount currency
            ]
        postRevenueRecognitionJV sapCfg token params tdsReimbursementLabel totals.reimbursementCount items currency reimbursementRows
  pure $ dedOk && reimbOk

-- 7. Subscription revenue recognised: Dr DEFERRED_REVENUE / Cr SUBSCRIPTION_REVENUE
--    Ride vs expiry use the same legs, different description labels.
dispatchSubscriptionRevenue ::
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
  Text ->
  Currency ->
  SubscriptionRevenueTotals ->
  [RevenueRecognitionTransactionRow] ->
  m Bool
dispatchSubscriptionRevenue _ _ _params label _currency totals _rows
  | totals.recognizedAmount == 0 = do
    logInfo $ "No " <> label <> " totals, skipping"
    pure True
dispatchSubscriptionRevenue sapCfg token params label currency totals rows = do
  let acctMap = sapCfg.accountMapping
  bId <- getNextBatchId
  items <-
    sequence
      [ mkItem bId "1" deferredRevenueAcct acctMap Debit totals.recognizedAmount currency,
        mkItem bId "2" subscriptionRevenueAcct acctMap Credit totals.recognizedAmount currency
      ]
  postRevenueRecognitionJV sapCfg token params label totals.txnCount items currency rows

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
  Currency ->
  [RevenueRecognitionTransactionRow] ->
  m ()
saveRevenueRecognitionTransactions mId mocId sapEntryId batchId label currency rows = do
  now <- getCurrentTime
  aInfo <- asks (.actorInfo)
  forM_ rows $ \row -> do
    txnId <- generateGUID
    QJETExtra.create
      JET.JournalEntryTransaction
        { id = Id txnId,
          debitAmount = row.amount,
          creditAmount = row.amount,
          currency,
          description = label,
          subscriptionId = Just row.referenceId, -- FIXME referenceId
          sapJournalEntryId = sapEntryId,
          sapBatchId = batchId,
          transactionType = SJE.RevenueRecognition,
          status = row.txnStatus,
          merchantId = mId.getId,
          merchantOperatingCityId = mocId.getId,
          createdAt = now,
          updatedAt = now,
          createdBy = aInfo.actorType,
          createdById = aInfo.actorId,
          updatedBy = aInfo.actorType,
          updatedById = aInfo.actorId
        }

-- ---------------------------------------------------------------------------
-- Helper functions
-- ---------------------------------------------------------------------------

mkSAPDispatchJobParams :: SAPRideRevenueDispatchJobData -> SAPDispatchJobParams
mkSAPDispatchJobParams SAPRideRevenueDispatchJobData {..} = SAPDispatchJobParams {..}

mkSAPRideRevenueDispatchJobData :: SAPDispatchJobParams -> SAPRideRevenueDispatchJobData
mkSAPRideRevenueDispatchJobData SAPDispatchJobParams {..} = SAPRideRevenueDispatchJobData {..}
