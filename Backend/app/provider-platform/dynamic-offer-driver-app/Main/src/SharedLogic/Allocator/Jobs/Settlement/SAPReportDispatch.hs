{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the

 GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
 -}

module SharedLogic.Allocator.Jobs.Settlement.SAPReportDispatch
  ( runSAPSubscriptionPurchaseDispatchJob,
    runSAPPGSettlementDispatchJob,
  )
where

import qualified Data.Map.Strict as M
import Kernel.Beam.Lib.UtilsTH (HasSchemaName)
import Kernel.External.Encryption ()
import qualified Kernel.External.SAP.Config as SAPConfig
import Kernel.External.SAP.Types (SAPJournalHeader (..), SAPJournalItem (..), SAPJournalRequest (..))
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Utils.Common
import qualified Lib.Finance.Core.Types as Finance
import qualified Lib.Finance.Domain.Types.SapJournalEntry as SJE
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import Lib.Scheduler
import Lib.Scheduler.JobStorageType.DB.Table (SchedulerJobT)
import qualified Lib.Scheduler.JobStorageType.SchedulerType as JC
import SharedLogic.Allocator (AllocatorJobType (..), SAPPGSettlementDispatchJobData (..), SAPSubscriptionPurchaseDispatchJobData (..))
import SharedLogic.Allocator.Jobs.Settlement.SAPDispatchCommon
import SharedLogic.Allocator.Jobs.Settlement.SubscriptionTotals (SubscriptionTotals (..), fetchPGSettlementTotals, fetchSubscriptionTotals)
import Storage.Beam.SchedulerJob ()

-- ---------------------------------------------------------------------------
-- Subscription Purchase Dispatch Job
-- ---------------------------------------------------------------------------

runSAPSubscriptionPurchaseDispatchJob ::
  (SAPJobConstraints m r c) =>
  Job 'SAPSubscriptionPurchaseDispatch ->
  m ExecutionResult
runSAPSubscriptionPurchaseDispatchJob Job {id, jobInfo} = do
  let jobData = jobInfo.jobData
  runSAPDispatchShell
    id.getId
    SAPDispatchShellCfg
      { lockKeyPrefix = "SAPSubscriptionPurchaseDispatch",
        idempotencyJobType = "SubscriptionPurchase",
        jobLabel = "subscription purchase"
      }
    (mkSAPDispatchJobParamsFromSubscription jobData)
    scheduleNextSubscriptionPurchaseJob
    ( \sapCfg token params -> do
        subTotals <- fetchSubscriptionTotals params.merchantOperatingCityId params.startTime params.endTime
        dispatchSubscriptionPurchase
          sapCfg
          token
          params.merchantId.getId
          params.merchantOperatingCityId.getId
          SubscriptionPurchase
          params.maxApiRetries
          params.startTime
          params.endTime
          subTotals
    )

-- ---------------------------------------------------------------------------
-- PG Settlement Dispatch Job
-- ---------------------------------------------------------------------------

runSAPPGSettlementDispatchJob ::
  (SAPJobConstraints m r c) =>
  Job 'SAPPGSettlementDispatch ->
  m ExecutionResult
runSAPPGSettlementDispatchJob Job {id, jobInfo} = do
  let jobData = jobInfo.jobData
  runSAPDispatchShell
    id.getId
    SAPDispatchShellCfg
      { lockKeyPrefix = "SAPPGSettlementDispatch",
        idempotencyJobType = "PGSettlement",
        jobLabel = "PG settlement"
      }
    (mkSAPDispatchJobParamsFromPGSettlement jobData)
    scheduleNextPGSettlementJob
    ( \sapCfg token params -> do
        let mId = params.merchantId
            mocid = params.merchantOperatingCityId
            retries = params.maxApiRetries
            fromTime = params.startTime
            toTime = params.endTime
        pgTotals <- fetchPGSettlementTotals mId.getId mocid fromTime toTime
        pgSettlementOrderOk <-
          dispatchEntry sapCfg token mId.getId mocid.getId retries PGSettlementOrder pgTotals.totalOrderAmount pgTotals.orderCount fromTime toTime
        refundOk <-
          dispatchEntry sapCfg token mId.getId mocid.getId retries RefundEntry pgTotals.totalRefundAmount pgTotals.refundCount fromTime toTime
        chargebackOk <-
          dispatchEntry sapCfg token mId.getId mocid.getId retries ChargebackEntry pgTotals.totalChargebackAmount pgTotals.chargebackCount fromTime toTime
        pure $ pgSettlementOrderOk && refundOk && chargebackOk
    )

-- ---------------------------------------------------------------------------
-- Domain helpers (subscription / PG settlement)
-- ---------------------------------------------------------------------------

dispatchEntry ::
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
  Text ->
  Text ->
  Int ->
  SAPEntryType ->
  HighPrecMoney ->
  Int ->
  UTCTime ->
  UTCTime ->
  m Bool
dispatchEntry _ _ _ _ _ entryType amount _ _ _
  | amount == 0 = do
    logInfo $ "No amount for " <> show entryType <> ", skipping"
    pure True
dispatchEntry sapCfg token mId mocid maxRetries entryType amount txnCount fromTime toTime = do
  let label = show entryType
  logInfo $ "Dispatching aggregated " <> label <> " entry to SAP, amount=" <> show amount <> " txnCount=" <> show txnCount
  req <- buildJournalRequest sapCfg entryType amount fromTime
  logInfo $ "SAP journal entry request body = " <> show req
  result <- callSAPWithRetry sapCfg token req label maxRetries
  handleSAPResponse label req result (toTransactionType entryType) txnCount mId mocid fromTime toTime

scheduleNextSubscriptionPurchaseJob ::
  ( BeamFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    JobCreatorEnv r,
    HasSchemaName SchedulerJobT,
    HasField "schedulerType" r SchedulerType
  ) =>
  NextSAPDispatchSchedule ->
  m ()
scheduleNextSubscriptionPurchaseJob NextSAPDispatchSchedule {scheduleAfter, jobParams} =
  JC.createJobIn @_ @'SAPSubscriptionPurchaseDispatch (Just jobParams.merchantId) (Just jobParams.merchantOperatingCityId) scheduleAfter (mkSAPSubscriptionPurchaseDispatchJobData jobParams)

scheduleNextPGSettlementJob ::
  ( BeamFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    JobCreatorEnv r,
    HasSchemaName SchedulerJobT,
    HasField "schedulerType" r SchedulerType
  ) =>
  NextSAPDispatchSchedule ->
  m ()
scheduleNextPGSettlementJob NextSAPDispatchSchedule {scheduleAfter, jobParams} =
  JC.createJobIn @_ @'SAPPGSettlementDispatch (Just jobParams.merchantId) (Just jobParams.merchantOperatingCityId) scheduleAfter (mkSAPPGSettlementDispatchJobData jobParams)

-- ---------------------------------------------------------------------------
-- Entry types
-- ---------------------------------------------------------------------------

data SAPEntryType
  = SubscriptionPurchase
  | PGSettlementOrder
  | RefundEntry
  | ChargebackEntry
  deriving (Show)

-- ---------------------------------------------------------------------------
-- Journal request builder
-- ---------------------------------------------------------------------------

buildJournalRequest ::
  (BeamFlow m r, CacheFlow m r) =>
  SAPConfig.SAPServiceConfig ->
  SAPEntryType ->
  HighPrecMoney ->
  UTCTime ->
  m SAPJournalRequest
buildJournalRequest sapCfg entryType amount fromTime = do
  now <- getCurrentTime
  let reqDate = formatSAPDate now
      reqTime = formatSAPTime now
      postingDate = formatSAPDate fromTime
      acctMap = sapCfg.accountMapping
      currency = "INR"
  bId <- getNextBatchId
  items <- filterZeroItems <$> buildItems entryType acctMap bId currency amount
  let header =
        SAPJournalHeader
          { msgtyp = Nothing,
            batchId = bId,
            requestDate = reqDate,
            requestTime = reqTime,
            headerdesc = show entryType,
            bukrs = sapCfg.bukrs,
            blart = sapCfg.blart,
            budat = postingDate,
            bldat = postingDate,
            attrName1 = Nothing,
            attrValue1 = Nothing,
            attrName2 = Nothing,
            attrValue2 = Nothing,
            attrName3 = Nothing,
            attrValue3 = Nothing,
            attrName4 = Nothing,
            attrValue4 = Nothing,
            attrName5 = Nothing,
            attrValue5 = Nothing,
            belnr = Nothing,
            gjahr = Nothing,
            message = Nothing,
            items = items
          }
  pure SAPJournalRequest {headers = [header]}

-- ---------------------------------------------------------------------------
-- Item builders per entry type
-- ---------------------------------------------------------------------------

buildItems ::
  (MonadFlow m) =>
  SAPEntryType ->
  M.Map Text SAPConfig.SAPAccountConfig ->
  Text ->
  Text ->
  HighPrecMoney ->
  m [SAPJournalItem]
buildItems SubscriptionPurchase _ _ _ _ = pure []
buildItems PGSettlementOrder acctMap bId currency amount =
  sequence
    [ mkItem bId "1" "BANK A/C" acctMap Debit amount currency,
      mkItem bId "2" "PG_CLEARING A/C" acctMap Credit amount currency
    ]
buildItems RefundEntry acctMap bId currency amount =
  sequence
    [ mkItem bId "1" "PG_CLEARING A/C" acctMap Debit amount currency,
      mkItem bId "2" "BANK A/C" acctMap Credit amount currency
    ]
buildItems ChargebackEntry acctMap bId currency amount =
  sequence
    [ mkItem bId "1" "PG_CLEARING A/C" acctMap Debit amount currency,
      mkItem bId "2" "BANK A/C" acctMap Credit amount currency
    ]

-- ---------------------------------------------------------------------------
-- Subscription purchase dispatch (aggregated)
-- ---------------------------------------------------------------------------

dispatchSubscriptionPurchase ::
  ( BeamFlow m r,
    EncFlow m r,
    CacheFlow m r,
    CoreMetrics m,
    Finance.HasActorInfo m r
  ) =>
  SAPConfig.SAPServiceConfig ->
  Text ->
  Text ->
  Text ->
  SAPEntryType ->
  Int ->
  UTCTime ->
  UTCTime ->
  SubscriptionTotals ->
  m Bool
dispatchSubscriptionPurchase _ _ _ _ _ _ _ _ totals
  | totals.grossAmount == 0 && totals.netAmount == 0 = do
    logInfo "No subscription purchase data found, skipping"
    pure True
dispatchSubscriptionPurchase sapCfg token mId mocid entryType maxRetries fromTime toTime totals = do
  logInfo $
    "Dispatching aggregated subscription purchase to SAP:"
      <> " grossAmount="
      <> show totals.grossAmount
      <> " cgst="
      <> show totals.cgst
      <> " sgst="
      <> show totals.sgst
      <> " igst="
      <> show totals.igst
      <> " netAmount="
      <> show totals.netAmount
  req <- buildSubscriptionJournalRequest sapCfg fromTime totals entryType
  result <- callSAPWithRetry sapCfg token req "SubscriptionPurchase" maxRetries
  handleSAPResponse "SubscriptionPurchase" req result SJE.SubscriptionPurchase totals.txnCount mId mocid fromTime toTime

buildSubscriptionJournalRequest ::
  (BeamFlow m r, CacheFlow m r) =>
  SAPConfig.SAPServiceConfig ->
  UTCTime ->
  SubscriptionTotals ->
  SAPEntryType ->
  m SAPJournalRequest
buildSubscriptionJournalRequest sapCfg fromTime totals entryType = do
  now <- getCurrentTime
  let reqDate = formatSAPDate now
      reqTime = formatSAPTime now
      postingDate = formatSAPDate fromTime
      acctMap = sapCfg.accountMapping
      currency = "INR"
  bId <- getNextBatchId
  filteredItems <-
    filterZeroItems
      <$> sequence
        [ mkItem bId "1" "PG_CLEARING A/C" acctMap Debit totals.grossAmount currency,
          mkItem bId "2" "DEFERRED_REVENUE A/C" acctMap Credit totals.netAmount currency,
          mkItem bId "3" "CGST_PAYABLE A/C" acctMap Credit totals.cgst currency,
          mkItem bId "4" "SGST_PAYABLE A/C" acctMap Credit totals.sgst currency,
          mkItem bId "5" "IGST_PAYABLE A/C" acctMap Credit totals.igst currency
        ]
  let header =
        SAPJournalHeader
          { msgtyp = Nothing,
            batchId = bId,
            requestDate = reqDate,
            requestTime = reqTime,
            headerdesc = show entryType,
            bukrs = sapCfg.bukrs,
            blart = sapCfg.blart,
            budat = postingDate,
            bldat = postingDate,
            attrName1 = Nothing,
            attrValue1 = Nothing,
            attrName2 = Nothing,
            attrValue2 = Nothing,
            attrName3 = Nothing,
            attrValue3 = Nothing,
            attrName4 = Nothing,
            attrValue4 = Nothing,
            attrName5 = Nothing,
            attrValue5 = Nothing,
            belnr = Nothing,
            gjahr = Nothing,
            message = Nothing,
            items = filteredItems
          }
      debit = totals.grossAmount
      credit = totals.netAmount + totals.cgst + totals.sgst + totals.igst
  assertDebitEqualsCredit "SubscriptionPurchase" bId debit credit
  pure SAPJournalRequest {headers = [header]}

-- ---------------------------------------------------------------------------
-- SAP Journal Entry persistence
-- ---------------------------------------------------------------------------

toTransactionType :: SAPEntryType -> SJE.TransactionType
toTransactionType SubscriptionPurchase = SJE.SubscriptionPurchase
toTransactionType PGSettlementOrder = SJE.Order
toTransactionType RefundEntry = SJE.Refund
toTransactionType ChargebackEntry = SJE.Chargeback

-- ---------------------------------------------------------------------------
-- Helper functions
-- ---------------------------------------------------------------------------

mkSAPDispatchJobParamsFromSubscription :: SAPSubscriptionPurchaseDispatchJobData -> SAPDispatchJobParams
mkSAPDispatchJobParamsFromSubscription SAPSubscriptionPurchaseDispatchJobData {..} = SAPDispatchJobParams {..}

mkSAPSubscriptionPurchaseDispatchJobData :: SAPDispatchJobParams -> SAPSubscriptionPurchaseDispatchJobData
mkSAPSubscriptionPurchaseDispatchJobData SAPDispatchJobParams {..} = SAPSubscriptionPurchaseDispatchJobData {..}

mkSAPDispatchJobParamsFromPGSettlement :: SAPPGSettlementDispatchJobData -> SAPDispatchJobParams
mkSAPDispatchJobParamsFromPGSettlement SAPPGSettlementDispatchJobData {..} = SAPDispatchJobParams {..}

mkSAPPGSettlementDispatchJobData :: SAPDispatchJobParams -> SAPPGSettlementDispatchJobData
mkSAPPGSettlementDispatchJobData SAPDispatchJobParams {..} = SAPPGSettlementDispatchJobData {..}
