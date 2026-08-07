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
    rideRevenueJvLabels,
  )
where

import Kernel.Beam.Lib.UtilsTH (HasSchemaName)
import Kernel.External.Encryption ()
import qualified Kernel.External.SAP.Config as SAPConfig
import Kernel.External.SAP.Types (SAPJournalItem (..))
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Utils.Common
import qualified Lib.Finance.Core.Types as Finance
import qualified Lib.Finance.Domain.Types.SapJournalEntry as SJE
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import Lib.Scheduler
import Lib.Scheduler.JobStorageType.DB.Table (SchedulerJobT)
import qualified Lib.Scheduler.JobStorageType.SchedulerType as JC
import SharedLogic.Allocator (AllocatorJobType (..), SAPRideRevenueDispatchJobData (..))
import SharedLogic.Allocator.Jobs.Settlement.RideRevenueTotals
import SharedLogic.Allocator.Jobs.Settlement.SAPDispatchCommon
import Storage.Beam.SchedulerJob ()

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

rideRevenueJvLabels :: [Text]
rideRevenueJvLabels =
  [ onlineRideRevRecLabel,
    offlineCashRideLabel,
    buyerAppSettlementLabel,
    driverEarningAccrualLabel,
    payoutToClearingLabel,
    payoutClearingToBankLabel,
    tdsDeductionLabel,
    tdsReimbursementLabel
  ]

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
  onlineOk <-
    dispatchRideFareRevRec
      sapCfg
      token
      params
      onlineRideRevRecLabel
      buyerAppReceivableAcct
      totals.onlineRideRevRec
  settleOk <- dispatchBuyerAppSettlement sapCfg token params totals.buyerAppSettlement
  offlineOk <-
    dispatchRideFareRevRec
      sapCfg
      token
      params
      offlineCashRideLabel
      driverBalanceAcct
      totals.offlineCashRide
  accrualOk <- dispatchDriverEarningAccrual sapCfg token params totals.driverEarningAccrual
  payoutOk <- dispatchPayout sapCfg token params totals.payout
  tdsOk <- dispatchTds sapCfg token params totals.tds
  pure $ onlineOk && settleOk && offlineOk && accrualOk && payoutOk && tdsOk

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
  m Bool
postRevenueRecognitionJV sapCfg token params label txnCount items = do
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
      handleSAPResponse label req result SJE.RevenueRecognition txnCount mId.getId mocid.getId fromTime toTime

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
  RideFareRevRecTotals ->
  m Bool
dispatchRideFareRevRec _sapCfg _token _params label _debitAcct totals
  | totals.grossAmount == 0 && totals.netAmount == 0 = do
    logInfo $ "No " <> label <> " totals, skipping"
    pure True
dispatchRideFareRevRec sapCfg token params label debitAcct totals = do
  let acctMap = sapCfg.accountMapping
      currency = "INR"
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
  postRevenueRecognitionJV sapCfg token params label totals.txnCount items

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
  BuyerAppSettlementTotals ->
  m Bool
dispatchBuyerAppSettlement _ _ _params totals
  | totals.settledAmount == 0 = do
    logInfo "No buyer-app settlement totals, skipping"
    pure True
dispatchBuyerAppSettlement sapCfg token params totals = do
  let acctMap = sapCfg.accountMapping
      currency = "INR" -- TODO remove hardcode
  bId <- getNextBatchId
  items <-
    sequence
      [ mkItem bId "1" bankAcct acctMap Debit totals.settledAmount currency,
        mkItem bId "2" buyerAppReceivableAcct acctMap Credit totals.settledAmount currency
      ]
  postRevenueRecognitionJV sapCfg token params buyerAppSettlementLabel totals.txnCount items

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
  DriverEarningAccrualTotals ->
  m Bool
dispatchDriverEarningAccrual _ _ _params totals
  | totals.accrualAmount == 0 = do
    logInfo "No driver earning accrual totals, skipping"
    pure True
dispatchDriverEarningAccrual sapCfg token params totals = do
  let acctMap = sapCfg.accountMapping
      currency = "INR"
  bId <- getNextBatchId
  items <-
    sequence
      [ mkItem bId "1" rideFareRevenueAcct acctMap Debit totals.accrualAmount currency,
        mkItem bId "2" driverBalanceAcct acctMap Credit totals.accrualAmount currency
      ]
  postRevenueRecognitionJV sapCfg token params driverEarningAccrualLabel totals.txnCount items

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
  PayoutTotals ->
  m Bool
dispatchPayout _ _ _params totals
  | totals.payoutAmount == 0 = do
    logInfo "No payout totals, skipping"
    pure True
dispatchPayout sapCfg token params totals = do
  let acctMap = sapCfg.accountMapping
      currency = "INR"
      amount = totals.payoutAmount
  bId1 <- getNextBatchId
  items1 <-
    sequence
      [ mkItem bId1 "1" driverBalanceAcct acctMap Debit amount currency,
        mkItem bId1 "2" payoutClearingAcct acctMap Credit amount currency
      ]
  ok1 <- postRevenueRecognitionJV sapCfg token params payoutToClearingLabel totals.txnCount items1
  -- JV2: Dr PAYOUT_CLEARING / Cr BANK
  bId2 <- getNextBatchId
  items2 <-
    sequence
      [ mkItem bId2 "1" payoutClearingAcct acctMap Debit amount currency,
        mkItem bId2 "2" bankAcct acctMap Credit amount currency
      ]
  ok2 <- postRevenueRecognitionJV sapCfg token params payoutClearingToBankLabel totals.txnCount items2
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
  TdsTotals ->
  m Bool
dispatchTds sapCfg token params totals = do
  dedOk <-
    if totals.deductionAmount == 0
      then do
        logInfo "No TDS deduction totals, skipping"
        pure True
      else do
        let acctMap = sapCfg.accountMapping
            currency = "INR"
        bId <- getNextBatchId
        items <-
          sequence
            [ mkItem bId "1" driverBalanceAcct acctMap Debit totals.deductionAmount currency,
              mkItem bId "2" tdsPayableAcct acctMap Credit totals.deductionAmount currency
            ]
        postRevenueRecognitionJV sapCfg token params tdsDeductionLabel totals.deductionCount items
  reimbOk <-
    if totals.reimbursementAmount == 0
      then do
        logInfo "No TDS reimbursement totals, skipping"
        pure True
      else do
        let acctMap = sapCfg.accountMapping
            currency = "INR"
        bId <- getNextBatchId
        items <-
          sequence
            [ mkItem bId "1" tdsReceivableAcct acctMap Debit totals.reimbursementAmount currency,
              mkItem bId "2" driverBalanceAcct acctMap Credit totals.reimbursementAmount currency
            ]
        postRevenueRecognitionJV sapCfg token params tdsReimbursementLabel totals.reimbursementCount items
  pure $ dedOk && reimbOk

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
scheduleNextRideRevenueJob NextSAPDispatchSchedule {scheduleAfter, jobParams} = do
  JC.createJobIn @_ @'SAPRideRevenueDispatch (Just jobParams.merchantId) (Just jobParams.merchantOperatingCityId) scheduleAfter (mkSAPRideRevenueDispatchJobData jobParams)

-- ---------------------------------------------------------------------------
-- Helper functions
-- ---------------------------------------------------------------------------

mkSAPDispatchJobParams :: SAPRideRevenueDispatchJobData -> SAPDispatchJobParams
mkSAPDispatchJobParams SAPRideRevenueDispatchJobData {..} = SAPDispatchJobParams {..}

mkSAPRideRevenueDispatchJobData :: SAPDispatchJobParams -> SAPRideRevenueDispatchJobData
mkSAPRideRevenueDispatchJobData SAPDispatchJobParams {..} = SAPRideRevenueDispatchJobData {..}
