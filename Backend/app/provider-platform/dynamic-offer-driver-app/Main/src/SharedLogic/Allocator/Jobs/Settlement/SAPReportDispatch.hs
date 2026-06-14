{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the

 GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
 -}

module SharedLogic.Allocator.Jobs.Settlement.SAPReportDispatch
  ( runSAPReportDispatchJob,
  )
where

import Control.Applicative ((<|>))
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Time (timeOfDayToTime)
import Data.Time.Calendar (addDays)
import Data.Time.Clock (UTCTime (..))
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.MerchantServiceConfig as DMSC
import qualified EulerHS.Language as L
import Kernel.Beam.Lib.UtilsTH (HasSchemaName)
import Kernel.External.Encryption ()
import qualified Kernel.External.SAP.Config as SAPConfig
import qualified Kernel.External.SAP.Interface as SAP
import Kernel.External.SAP.Types (SAPJournalHeader (..), SAPJournalItem (..), SAPJournalRequest (..), SAPJournalResponse (..))
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common
import qualified Lib.Finance.Domain.Types.PgPaymentSettlementReport as Dom
import qualified Lib.Finance.Domain.Types.SapJournalEntry as SJE
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Finance.Storage.Queries.SapJournalEntry as QSJE
import Lib.Scheduler
import Lib.Scheduler.JobStorageType.DB.Table (SchedulerJobT)
import qualified Lib.Scheduler.JobStorageType.SchedulerType as JC
import SharedLogic.Allocator (AllocatorJobType (..), SAPReportDispatchJobData (..))
import SharedLogic.Allocator.Jobs.Settlement.SubscriptionTotals (SubscriptionTotals (..), fetchPgReportsInBatches, fetchSubscriptionTotals)
import Storage.Beam.SchedulerJob ()
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as CQMSC
import Tools.Error

lockTTLSeconds :: Int
lockTTLSeconds = 600

runSAPReportDispatchJob ::
  ( BeamFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    MonadFlow m,
    MonadIO m,
    CoreMetrics m,
    L.MonadFlow m,
    HasRequestId r,
    MonadReader r m,
    HasShortDurationRetryCfg r c,
    HasField "maxShards" r Int,
    HasField "schedulerSetName" r Text,
    HasField "schedulerType" r SchedulerType,
    HasField "jobInfoMap" r (M.Map Text Bool),
    HasField "blackListedJobs" r [Text],
    JobCreatorEnv r,
    HasSchemaName SchedulerJobT
  ) =>
  Job 'SAPReportDispatch ->
  m ExecutionResult
runSAPReportDispatchJob Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
      merchantId = jobData.merchantId
      merchantOperatingCityId = jobData.merchantOperatingCityId

  let lockKey = "SAPReportDispatch:" <> merchantId.getId <> ":" <> merchantOperatingCityId.getId

  mbResult <- Hedis.whenWithLockRedisAndReturnValue lockKey lockTTLSeconds $ do
    logInfo "Starting SAP report dispatch"

    mbSAPConfig <- getSAPConfig merchantOperatingCityId
    case mbSAPConfig of
      Nothing -> do
        logWarning "No SAP config found in MerchantServiceConfig"
        pure True
      Just sapCfg -> do
        now <- getCurrentTime
        let today = utctDay now
            yesterday = addDays (-1) today
            fromTime = UTCTime yesterday 0
            toTime = addUTCTime (-1) (UTCTime today 0)

        let retries = jobData.maxApiRetries

        tokenResult <- fetchSAPTokenWithRetry sapCfg retries
        case tokenResult of
          Left err -> do
            logError $ "SAP token fetch failed after " <> show retries <> " retries: " <> err
            pure False
          Right token -> do
            result <- try @_ @SomeException $ do
              subTotals <- fetchSubscriptionTotals merchantOperatingCityId fromTime toTime
              subPurchaseOk <- dispatchSubscriptionPurchase sapCfg token merchantId.getId merchantOperatingCityId.getId retries subTotals

              reports <- fetchPgReportsInBatches merchantId.getId fromTime toTime
              let orders = filter (\r -> r.txnType == Dom.ORDER) reports
                  refunds = filter (\r -> r.txnType == Dom.REFUND) reports
                  chargebacks = filter (\r -> r.txnType == Dom.CHARGEBACK) reports

              pgSettlementOrderOk <- dispatchEntry sapCfg token merchantId.getId merchantOperatingCityId.getId retries PGSettlementOrder orders
              refundOk <- dispatchEntry sapCfg token merchantId.getId merchantOperatingCityId.getId retries RefundEntry refunds
              chargebackOk <- dispatchEntry sapCfg token merchantId.getId merchantOperatingCityId.getId retries ChargebackEntry chargebacks

              pure $ subPurchaseOk && pgSettlementOrderOk && refundOk && chargebackOk
            case result of
              Left err -> do
                logError $ "SAP dispatch failed with exception: " <> show err
                pure False
              Right ok -> pure ok

  case mbResult of
    Left () -> do
      logWarning $ "SAP dispatch lock contention, will retry: " <> lockKey
      pure Retry
    Right allSucceeded -> do
      scheduleNextDispatchJob merchantId merchantOperatingCityId jobData.scheduledTime jobData.timeDiffFromUtc jobData.maxApiRetries
      if allSucceeded
        then pure Complete
        else do
          logWarning "Some SAP dispatches had failures, scheduling next run anyway"
          pure Complete
  where
    getSAPConfig ::
      (BeamFlow m r, CacheFlow m r, EsqDBFlow m r) =>
      Id DMOC.MerchantOperatingCity ->
      m (Maybe SAPConfig.SAPServiceConfig)
    getSAPConfig mocid = do
      mbConfig <- CQMSC.findByServiceAndCity (DMSC.SAPService DMSC.Journal) mocid
      pure $ case mbConfig of
        Just cfg -> case cfg.serviceConfig of
          DMSC.SAPServiceConfig sapCfg -> Just sapCfg
          _ -> Nothing
        Nothing -> Nothing

    dispatchEntry ::
      ( BeamFlow m r,
        EncFlow m r,
        CacheFlow m r,
        CoreMetrics m,
        MonadFlow m,
        HasRequestId r,
        MonadReader r m
      ) =>
      SAPConfig.SAPServiceConfig ->
      Text ->
      Text ->
      Text ->
      Int ->
      SAPEntryType ->
      [Dom.PgPaymentSettlementReport] ->
      m Bool
    dispatchEntry _ _ _ _ _ entryType [] = do
      logInfo $ "No reports for " <> show entryType <> ", skipping"
      pure True
    dispatchEntry sapCfg token mId mocid maxRetries entryType reports = do
      let label = show entryType
      logInfo $ "Dispatching " <> show (length reports) <> " " <> label <> " entries to SAP"
      req <- buildJournalRequest sapCfg entryType reports
      result <- callSAPWithRetry sapCfg token req label maxRetries
      case result of
        Left err -> do
          logError $ "SAP " <> label <> " dispatch failed after " <> show maxRetries <> " retries: " <> err
          saveSapJournalEntries req Nothing SJE.FAILED (toTransactionType entryType) (length reports) mId mocid (Just err)
          pure False
        Right resp -> do
          forM_ resp.responseHeaders $ \hdr ->
            logInfo $ "SAP " <> label <> " dispatch response: batchId=" <> fromMaybe "" hdr.batchId <> " msgtyp=" <> fromMaybe "" hdr.msgtyp
          saveSapJournalEntries req (Just resp) SJE.SUCCESS (toTransactionType entryType) (length reports) mId mocid Nothing
          pure True

    scheduleNextDispatchJob ::
      ( BeamFlow m r,
        CacheFlow m r,
        EsqDBFlow m r,
        JobCreatorEnv r,
        HasSchemaName SchedulerJobT,
        HasField "schedulerType" r SchedulerType
      ) =>
      Id DM.Merchant ->
      Id DMOC.MerchantOperatingCity ->
      TimeOfDay ->
      Seconds ->
      Int ->
      m ()
    scheduleNextDispatchJob mId mocid scheduledTime' utcOffset maxRetries = do
      now <- getCurrentTime
      let todayDay = utctDay now
          tomorrowDay = addDays 1 todayDay
          tomorrowRunTime = addUTCTime (fromIntegral $ negate utcOffset) $ UTCTime tomorrowDay (timeOfDayToTime scheduledTime')
          scheduleAfter = diffUTCTime tomorrowRunTime now
      logInfo $ "Scheduling next SAP dispatch in " <> show scheduleAfter
      JC.createJobIn @_ @'SAPReportDispatch (Just mId) (Just mocid) scheduleAfter $
        SAPReportDispatchJobData {merchantId = mId, merchantOperatingCityId = mocid, scheduledTime = scheduledTime', timeDiffFromUtc = utcOffset, maxApiRetries = maxRetries}

-- ---------------------------------------------------------------------------
-- Entry types
-- ---------------------------------------------------------------------------

data SAPEntryType
  = SubscriptionPurchase
  | PGSettlementOrder
  | RefundEntry
  | ChargebackEntry
  deriving (Show)

data PostingDirection = Debit | Credit

toShkzg :: PostingDirection -> Text
toShkzg Debit = "S"
toShkzg Credit = "H"

-- ---------------------------------------------------------------------------
-- Redis batch-id counter
-- ---------------------------------------------------------------------------

sapBatchIdCounterKey :: Text
sapBatchIdCounterKey = "SAPReportDispatch:BatchIdCounter"

getNextBatchId :: (CacheFlow m r) => m Text
getNextBatchId = show <$> Hedis.incr sapBatchIdCounterKey

-- ---------------------------------------------------------------------------
-- Journal request builder
-- ---------------------------------------------------------------------------

buildJournalRequest ::
  (MonadFlow m, CacheFlow m r) =>
  SAPConfig.SAPServiceConfig ->
  SAPEntryType ->
  [Dom.PgPaymentSettlementReport] ->
  m SAPJournalRequest
buildJournalRequest sapCfg entryType reports = do
  now <- getCurrentTime
  let reqDate = show (utctDay now)
      reqTime = show (utctDayTime now)
  headers <- forM reports $ \report -> do
    when (isNothing report.txnDate) $
      throwError (InternalError $ "SAP " <> show entryType <> " report missing txnDate, orderId: " <> report.orderId)
    bId <- getNextBatchId
    let hdr = buildHeader sapCfg entryType reqDate reqTime bId report
        filteredItems = filterZeroItems hdr.items
    pure hdr {items = filteredItems}
  pure SAPJournalRequest {headers}

-- ---------------------------------------------------------------------------
-- Header builder
-- ---------------------------------------------------------------------------

buildHeader ::
  SAPConfig.SAPServiceConfig ->
  SAPEntryType ->
  Text ->
  Text ->
  Text ->
  Dom.PgPaymentSettlementReport ->
  SAPJournalHeader
buildHeader sapCfg entryType reqDate reqTime bId report =
  let txnDateStr = maybe "" (show . utctDay) report.txnDate
      acctMap = sapCfg.accountMapping
      currency = show report.currency
   in SAPJournalHeader
        { msgtyp = Nothing,
          batchId = bId,
          requestDate = reqDate,
          requestTime = reqTime,
          headerdesc = show entryType,
          bukrs = sapCfg.bukrs,
          blart = sapCfg.blart,
          budat = txnDateStr,
          bldat = txnDateStr,
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
          items = buildItems entryType acctMap bId currency report
        }

-- ---------------------------------------------------------------------------
-- Item builders per entry type
-- ---------------------------------------------------------------------------

buildItems ::
  SAPEntryType ->
  M.Map Text SAPConfig.SAPAccountConfig ->
  Text ->
  Text ->
  Dom.PgPaymentSettlementReport ->
  [SAPJournalItem]
buildItems SubscriptionPurchase _ _ _ _ = []
buildItems PGSettlementOrder acctMap bId currency report =
  let grossAmount = report.txnAmount
   in [ mkItem bId "1" "BANK A/C" acctMap Debit grossAmount currency,
        mkItem bId "2" "PG_CLEARING A/C" acctMap Credit grossAmount currency
      ]
buildItems RefundEntry acctMap bId currency report =
  let refundAmount = fromMaybe 0 report.refundAmount
   in [ mkItem bId "1" "PG_CLEARING A/C" acctMap Debit refundAmount currency,
        mkItem bId "2" "BANK A/C" acctMap Credit refundAmount currency
      ]
buildItems ChargebackEntry acctMap bId currency report =
  let chargebackAmount = fromMaybe 0 report.chargebackAmount
   in [ mkItem bId "1" "PG_CLEARING A/C" acctMap Debit chargebackAmount currency,
        mkItem bId "2" "BANK A/C" acctMap Credit chargebackAmount currency
      ]

-- ---------------------------------------------------------------------------
-- Single item helper
-- ---------------------------------------------------------------------------

mkItem ::
  Text ->
  Text ->
  Text ->
  M.Map Text SAPConfig.SAPAccountConfig ->
  PostingDirection ->
  HighPrecMoney ->
  Text ->
  SAPJournalItem
mkItem bId itemNum acctKey acctMap direction amount currency =
  let mbAcct = M.lookup acctKey acctMap
   in SAPJournalItem
        { batchId = bId,
          batchItem = itemNum,
          itemdesc = acctKey,
          hkont = maybe "" (.hkont) mbAcct,
          amount = show amount,
          shkzg = toShkzg direction,
          kostl = toKostl direction mbAcct,
          prctr = toPrctr direction mbAcct,
          waers = currency,
          attrName1 = Nothing,
          attrValue1 = Nothing,
          attrName2 = Nothing,
          attrValue2 = Nothing,
          attrName3 = Nothing,
          attrValue3 = Nothing,
          attrName4 = Nothing,
          attrValue4 = Nothing,
          attrName5 = Nothing,
          attrValue5 = Nothing
        }

toKostl :: PostingDirection -> Maybe SAPConfig.SAPAccountConfig -> Maybe Text
toKostl Debit mbAcct = mbAcct >>= (.kostl)
toKostl Credit _ = Nothing

toPrctr :: PostingDirection -> Maybe SAPConfig.SAPAccountConfig -> Maybe Text
toPrctr Credit mbAcct = mbAcct >>= (.prctr)
toPrctr Debit _ = Nothing

-- ---------------------------------------------------------------------------
-- Subscription purchase dispatch (aggregated)
-- ---------------------------------------------------------------------------

dispatchSubscriptionPurchase ::
  ( BeamFlow m r,
    EncFlow m r,
    CacheFlow m r,
    CoreMetrics m,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  SAPConfig.SAPServiceConfig ->
  Text ->
  Text ->
  Text ->
  Int ->
  SubscriptionTotals ->
  m Bool
dispatchSubscriptionPurchase _ _ _ _ _ totals
  | totals.grossAmount == 0 && totals.netAmount == 0 = do
    logInfo "No subscription purchase data found, skipping"
    pure True
dispatchSubscriptionPurchase sapCfg token mId mocid maxRetries totals = do
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
  req <- buildSubscriptionJournalRequest sapCfg totals
  result <- callSAPWithRetry sapCfg token req "SubscriptionPurchase" maxRetries
  case result of
    Left err -> do
      logError $ "SAP SubscriptionPurchase dispatch failed after " <> show maxRetries <> " retries: " <> err
      saveSapJournalEntries req Nothing SJE.FAILED SJE.SubscriptionPurchase 1 mId mocid (Just err)
      pure False
    Right resp -> do
      logInfo $ "SAP SubscriptionPurchase dispatch response: " <> show resp
      saveSapJournalEntries req (Just resp) SJE.SUCCESS SJE.SubscriptionPurchase 1 mId mocid Nothing
      pure True

buildSubscriptionJournalRequest ::
  (MonadFlow m, CacheFlow m r) =>
  SAPConfig.SAPServiceConfig ->
  SubscriptionTotals ->
  m SAPJournalRequest
buildSubscriptionJournalRequest sapCfg totals = do
  now <- getCurrentTime
  let reqDate = show (utctDay now)
      reqTime = show (utctDayTime now)
      yesterday = show (addDays (-1) (utctDay now))
      acctMap = sapCfg.accountMapping
      currency = "INR"
  bId <- getNextBatchId
  let header =
        SAPJournalHeader
          { msgtyp = Nothing,
            batchId = bId,
            requestDate = reqDate,
            requestTime = reqTime,
            headerdesc = "Online Subscription Sale",
            bukrs = sapCfg.bukrs,
            blart = sapCfg.blart,
            budat = yesterday,
            bldat = yesterday,
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
      filteredItems =
        filterZeroItems
          [ mkItem bId "1" "PG_CLEARING A/C" acctMap Debit totals.grossAmount currency,
            mkItem bId "2" "DEFERRED_REVENUE A/C" acctMap Credit totals.netAmount currency,
            mkItem bId "3" "CGST_PAYABLE A/C" acctMap Credit totals.cgst currency,
            mkItem bId "4" "SGST_PAYABLE A/C" acctMap Credit totals.sgst currency,
            mkItem bId "5" "IGST_PAYABLE A/C" acctMap Credit totals.igst currency
          ]
      debit = totals.grossAmount
      credit = totals.netAmount + totals.cgst + totals.sgst + totals.igst
  when (debit /= credit) $
    throwError (InternalError $ "SAP SubscriptionPurchase debit/credit mismatch: debit=" <> show debit <> " credit=" <> show credit <> " batchId=" <> bId)
  pure SAPJournalRequest {headers = [header]}

-- ---------------------------------------------------------------------------
-- SAP Journal Entry persistence
-- ---------------------------------------------------------------------------

toTransactionType :: SAPEntryType -> SJE.TransactionType
toTransactionType SubscriptionPurchase = SJE.SubscriptionPurchase
toTransactionType PGSettlementOrder = SJE.Order
toTransactionType RefundEntry = SJE.Refund
toTransactionType ChargebackEntry = SJE.Chargeback

saveSapJournalEntries ::
  (BeamFlow m r, MonadFlow m) =>
  SAPJournalRequest ->
  Maybe SAPJournalResponse ->
  SJE.JournalEntryStatus ->
  SJE.TransactionType ->
  Int ->
  Text ->
  Text ->
  Maybe Text ->
  m ()
saveSapJournalEntries req mbResp entryStatus txnType txnCount mId mocid mbErrMsg = do
  now <- getCurrentTime
  let reqHeaders = req.headers
      respHeaders = maybe (repeat Nothing) (map Just . (.responseHeaders)) mbResp
  forM_ (zip reqHeaders respHeaders) $ \(reqHeader, mbRespHeader) -> do
    entryId <- generateGUID
    let (totalDebit, totalCredit) = computeDebitCreditTotals reqHeader.items
    QSJE.create
      SJE.SapJournalEntry
        { id = Id entryId,
          belnr = mbRespHeader >>= (.belnr),
          batchId = reqHeader.batchId,
          blart = reqHeader.blart,
          transactionType = txnType,
          description = reqHeader.headerdesc,
          budat = reqHeader.budat,
          bldat = reqHeader.bldat,
          gjahr = mbRespHeader >>= (.gjahr),
          totalDebitAmount = totalDebit,
          totalCreditAmount = totalCredit,
          currency = INR,
          transactionCount = txnCount,
          glNumber = Just $ map (.hkont) reqHeader.items,
          glName = Just $ map (.itemdesc) reqHeader.items,
          sapMessage = (mbRespHeader >>= (.message)) <|> mbErrMsg,
          status = entryStatus,
          rawResponse = (.rawXml) <$> mbResp,
          merchantId = mId,
          merchantOperatingCityId = mocid,
          createdAt = now,
          updatedAt = now
        }

filterZeroItems :: [SAPJournalItem] -> [SAPJournalItem]
filterZeroItems = filter (\item -> parseItemAmount item /= 0)

parseItemAmount :: SAPJournalItem -> HighPrecMoney
parseItemAmount item = fromMaybe 0 (readMaybe (T.unpack item.amount) :: Maybe HighPrecMoney)

computeDebitCreditTotals :: [SAPJournalItem] -> (HighPrecMoney, HighPrecMoney)
computeDebitCreditTotals items =
  let debitTotal = sum [parseItemAmount item | item <- items, item.shkzg == "S"]
      creditTotal = sum [parseItemAmount item | item <- items, item.shkzg == "H"]
   in (debitTotal, creditTotal)

-- ---------------------------------------------------------------------------
-- SAP API call with retry
-- ---------------------------------------------------------------------------

fetchSAPTokenWithRetry ::
  ( EncFlow m r,
    CoreMetrics m,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  SAPConfig.SAPServiceConfig ->
  Int ->
  m (Either Text Text)
fetchSAPTokenWithRetry sapCfg maxRetries = go 0
  where
    go attempt = do
      result <- try @_ @SomeException $ SAP.fetchSAPToken sapCfg
      case result of
        Right resp -> pure $ Right resp.access_token
        Left err -> do
          let attemptsLeft = maxRetries - attempt - 1
          if attemptsLeft > 0
            then do
              logWarning $ "SAP token fetch attempt " <> show (attempt + 1) <> " failed: " <> show err <> ", retrying (" <> show attemptsLeft <> " left)"
              go (attempt + 1)
            else pure $ Left (show err)

callSAPWithRetry ::
  ( EncFlow m r,
    CacheFlow m r,
    CoreMetrics m,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  SAPConfig.SAPServiceConfig ->
  Text ->
  SAPJournalRequest ->
  Text ->
  Int ->
  m (Either Text SAPJournalResponse)
callSAPWithRetry sapCfg token req label maxRetries = go 0
  where
    go attempt = do
      result <- try @_ @SomeException $ SAP.postJournalEntry sapCfg token req
      case result of
        Right resp -> pure $ Right resp
        Left err -> do
          let attemptsLeft = maxRetries - attempt - 1
          if attemptsLeft > 0
            then do
              logWarning $ "SAP " <> label <> " attempt " <> show (attempt + 1) <> " failed: " <> show err <> ", retrying (" <> show attemptsLeft <> " left)"
              go (attempt + 1)
            else pure $ Left (show err)
