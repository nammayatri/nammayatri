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

import Control.Applicative ((<|>))
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Time (timeOfDayToTime)
import Data.Time.Calendar (addDays)
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)
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
import qualified Lib.Finance.Domain.Types.SapJournalEntry as SJE
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Finance.Storage.Queries.SapJournalEntry as QSJE
import Lib.Scheduler
import Lib.Scheduler.JobStorageType.DB.Table (SchedulerJobT)
import qualified Lib.Scheduler.JobStorageType.SchedulerType as JC
import SharedLogic.Allocator (AllocatorJobType (..), SAPPGSettlementDispatchJobData (..), SAPSubscriptionPurchaseDispatchJobData (..))
import SharedLogic.Allocator.Jobs.Settlement.SubscriptionTotals (SubscriptionTotals (..), fetchPGSettlementTotals, fetchSubscriptionTotals)
import Storage.Beam.SchedulerJob ()
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as CQMSC
import Tools.Error

lockTTLSeconds :: Int
lockTTLSeconds = 600

idempotencyTTLSeconds :: Int
idempotencyTTLSeconds = 86400

mkIdempotencyKey :: Text -> Text -> Text -> Text
mkIdempotencyKey jobType mocId dateStr = "SAP:Idempotency:" <> jobType <> ":" <> mocId <> ":" <> dateStr

isAlreadyDispatched :: (CacheFlow m r) => Text -> m Bool
isAlreadyDispatched key = do
  mbVal <- Hedis.get key
  pure $ isJust (mbVal :: Maybe Text)

markAsDispatched :: (CacheFlow m r) => Text -> m ()
markAsDispatched key = Hedis.setExp key ("1" :: Text) idempotencyTTLSeconds

-- ---------------------------------------------------------------------------
-- Common job constraints
-- ---------------------------------------------------------------------------

type SAPJobConstraints m r c =
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
  )

-- ---------------------------------------------------------------------------
-- Subscription Purchase Dispatch Job
-- ---------------------------------------------------------------------------

runSAPSubscriptionPurchaseDispatchJob ::
  (SAPJobConstraints m r c) =>
  Job 'SAPSubscriptionPurchaseDispatch ->
  m ExecutionResult
runSAPSubscriptionPurchaseDispatchJob Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
      merchantId = jobData.merchantId
      merchantOperatingCityId = jobData.merchantOperatingCityId

  let lockKey = "SAPSubscriptionPurchaseDispatch:" <> merchantId.getId <> ":" <> merchantOperatingCityId.getId
      fromTime = jobData.startTime
      toTime = jobData.endTime
      idempotencyKey = mkIdempotencyKey "SubscriptionPurchase" merchantOperatingCityId.getId (show $ utctDay fromTime)

  let shouldScheduleNext = fromMaybe True jobData.scheduleNextJob

  dispatched <- isAlreadyDispatched idempotencyKey
  if dispatched
    then do
      logInfo $ "SAP subscription purchase already dispatched for " <> show (utctDay fromTime) <> ", skipping"
      when shouldScheduleNext $
        scheduleNextSubscriptionPurchaseJob merchantId merchantOperatingCityId jobData.scheduledTime jobData.timeDiffFromUtc jobData.maxApiRetries
      pure Complete
    else do
      mbResult <- Hedis.whenWithLockRedisAndReturnValue lockKey lockTTLSeconds $ do
        logInfo "Starting SAP subscription purchase dispatch"

        mbSAPConfig <- getSAPConfig merchantOperatingCityId
        case mbSAPConfig of
          Nothing -> do
            logWarning "No SAP config found in MerchantServiceConfig"
            pure True
          Just sapCfg -> do
            let retries = jobData.maxApiRetries

            tokenResult <- fetchSAPTokenWithRetry sapCfg retries
            case tokenResult of
              Left err -> do
                logError $ "SAP token fetch failed after " <> show retries <> " retries: " <> err
                pure False
              Right token -> do
                result <- try @_ @SomeException $ do
                  subTotals <- fetchSubscriptionTotals merchantOperatingCityId fromTime toTime
                  dispatchSubscriptionPurchase sapCfg token merchantId.getId merchantOperatingCityId.getId SubscriptionPurchase retries fromTime toTime subTotals
                case result of
                  Left err -> do
                    logError $ "SAP subscription purchase dispatch failed with exception: " <> show err
                    pure False
                  Right ok -> do
                    when ok $ markAsDispatched idempotencyKey
                    pure ok

      case mbResult of
        Left () -> do
          logWarning $ "SAP subscription purchase dispatch lock contention, will retry: " <> lockKey
          pure Retry
        Right succeeded -> do
          when shouldScheduleNext $
            scheduleNextSubscriptionPurchaseJob merchantId merchantOperatingCityId jobData.scheduledTime jobData.timeDiffFromUtc jobData.maxApiRetries
          if succeeded
            then pure Complete
            else do
              logWarning "SAP subscription purchase dispatch had failures, scheduling next run anyway"
              pure Complete

-- ---------------------------------------------------------------------------
-- PG Settlement Dispatch Job
-- ---------------------------------------------------------------------------

runSAPPGSettlementDispatchJob ::
  (SAPJobConstraints m r c) =>
  Job 'SAPPGSettlementDispatch ->
  m ExecutionResult
runSAPPGSettlementDispatchJob Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
      merchantId = jobData.merchantId
      merchantOperatingCityId = jobData.merchantOperatingCityId

  let lockKey = "SAPPGSettlementDispatch:" <> merchantId.getId <> ":" <> merchantOperatingCityId.getId
      fromTime = jobData.startTime
      toTime = jobData.endTime
      idempotencyKey = mkIdempotencyKey "PGSettlement" merchantOperatingCityId.getId (show $ utctDay fromTime)

  let shouldScheduleNext = fromMaybe True jobData.scheduleNextJob

  dispatched <- isAlreadyDispatched idempotencyKey
  if dispatched
    then do
      logInfo $ "SAP PG settlement already dispatched for " <> show (utctDay fromTime) <> ", skipping"
      when shouldScheduleNext $
        scheduleNextPGSettlementJob merchantId merchantOperatingCityId jobData.scheduledTime jobData.timeDiffFromUtc jobData.maxApiRetries
      pure Complete
    else do
      mbResult <- Hedis.whenWithLockRedisAndReturnValue lockKey lockTTLSeconds $ do
        logInfo "Starting SAP PG settlement dispatch"

        mbSAPConfig <- getSAPConfig merchantOperatingCityId
        case mbSAPConfig of
          Nothing -> do
            logWarning "No SAP config found in MerchantServiceConfig"
            pure True
          Just sapCfg -> do
            let retries = jobData.maxApiRetries

            tokenResult <- fetchSAPTokenWithRetry sapCfg retries
            case tokenResult of
              Left err -> do
                logError $ "SAP token fetch failed after " <> show retries <> " retries: " <> err
                pure False
              Right token -> do
                result <- try @_ @SomeException $ do
                  pgTotals <- fetchPGSettlementTotals merchantId.getId merchantOperatingCityId fromTime toTime
                  pgSettlementOrderOk <- dispatchEntry sapCfg token merchantId.getId merchantOperatingCityId.getId retries PGSettlementOrder pgTotals.totalOrderAmount pgTotals.orderCount fromTime toTime
                  refundOk <- dispatchEntry sapCfg token merchantId.getId merchantOperatingCityId.getId retries RefundEntry pgTotals.totalRefundAmount pgTotals.refundCount fromTime toTime
                  chargebackOk <- dispatchEntry sapCfg token merchantId.getId merchantOperatingCityId.getId retries ChargebackEntry pgTotals.totalChargebackAmount pgTotals.chargebackCount fromTime toTime

                  pure $ pgSettlementOrderOk && refundOk && chargebackOk
                case result of
                  Left err -> do
                    logError $ "SAP PG settlement dispatch failed with exception: " <> show err
                    pure False
                  Right ok -> do
                    when ok $ markAsDispatched idempotencyKey
                    pure ok

      case mbResult of
        Left () -> do
          logWarning $ "SAP PG settlement dispatch lock contention, will retry: " <> lockKey
          pure Retry
        Right allSucceeded -> do
          when shouldScheduleNext $
            scheduleNextPGSettlementJob merchantId merchantOperatingCityId jobData.scheduledTime jobData.timeDiffFromUtc jobData.maxApiRetries
          if allSucceeded
            then pure Complete
            else do
              logWarning "Some SAP PG settlement dispatches had failures, scheduling next run anyway"
              pure Complete

-- ---------------------------------------------------------------------------
-- Common helpers
-- ---------------------------------------------------------------------------

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
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  TimeOfDay ->
  Seconds ->
  Int ->
  m ()
scheduleNextSubscriptionPurchaseJob mId mocid scheduledTime' utcOffset maxRetries = do
  now <- getCurrentTime
  let istOffset = secondsToNominalDiffTime utcOffset
      nowIST = addUTCTime istOffset now
      todayDayIST = utctDay nowIST
      tomorrowDayIST = addDays 1 todayDayIST
      tomorrowRunTime = addUTCTime (negate istOffset) $ UTCTime tomorrowDayIST (timeOfDayToTime scheduledTime')
      scheduleAfter = diffUTCTime tomorrowRunTime now
      nextStartTime = addUTCTime (negate istOffset) $ UTCTime todayDayIST 0
      nextEndTime = addUTCTime (negate istOffset) $ UTCTime todayDayIST (secondsToDiffTime 86399)
  logInfo $ "Scheduling next SAP subscription purchase dispatch in " <> show scheduleAfter
  JC.createJobIn @_ @'SAPSubscriptionPurchaseDispatch (Just mId) (Just mocid) scheduleAfter $
    SAPSubscriptionPurchaseDispatchJobData {merchantId = mId, merchantOperatingCityId = mocid, scheduledTime = scheduledTime', timeDiffFromUtc = utcOffset, maxApiRetries = maxRetries, startTime = nextStartTime, endTime = nextEndTime, scheduleNextJob = Just True}

scheduleNextPGSettlementJob ::
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
scheduleNextPGSettlementJob mId mocid scheduledTime' utcOffset maxRetries = do
  now <- getCurrentTime
  let istOffset = secondsToNominalDiffTime utcOffset
      nowIST = addUTCTime istOffset now
      todayDayIST = utctDay nowIST
      tomorrowDayIST = addDays 1 todayDayIST
      tomorrowRunTime = addUTCTime (negate istOffset) $ UTCTime tomorrowDayIST (timeOfDayToTime scheduledTime')
      scheduleAfter = diffUTCTime tomorrowRunTime now
      nextStartTime = addUTCTime (negate istOffset) $ UTCTime todayDayIST 0
      nextEndTime = addUTCTime (negate istOffset) $ UTCTime todayDayIST (secondsToDiffTime 86399)
  logInfo $ "Scheduling next SAP PG settlement dispatch in " <> show scheduleAfter
  JC.createJobIn @_ @'SAPPGSettlementDispatch (Just mId) (Just mocid) scheduleAfter $
    SAPPGSettlementDispatchJobData {merchantId = mId, merchantOperatingCityId = mocid, scheduledTime = scheduledTime', timeDiffFromUtc = utcOffset, maxApiRetries = maxRetries, startTime = nextStartTime, endTime = nextEndTime, scheduleNextJob = Just True}

-- ---------------------------------------------------------------------------
-- Entry types
-- ---------------------------------------------------------------------------

data SAPEntryType
  = SubscriptionPurchase
  | PGSettlementOrder
  | RefundEntry
  | ChargebackEntry
  deriving (Show)

-- | Human-readable description sent to SAP (headerdesc) and stored on the
-- journal entry. Kept distinct from the `Show` instance (constructor names) so
-- the values match the finance spec / dashboard, not the Haskell type.
entryDescription :: SAPEntryType -> Text
entryDescription SubscriptionPurchase = "Online Subscription Sale"
entryDescription PGSettlementOrder = "Online Subscription PG Settlement"
entryDescription RefundEntry = "Online Subscription Refund"
entryDescription ChargebackEntry = "Online Subscription Chargeback"

data PostingDirection = Debit | Credit

toShkzg :: PostingDirection -> Text
toShkzg Debit = "S"
toShkzg Credit = "H"

-- ---------------------------------------------------------------------------
-- Redis batch-id counter
-- ---------------------------------------------------------------------------

sapBatchIdCounterKey :: Text
sapBatchIdCounterKey = "SAPReportDispatch:BatchIdCounter"

sapBatchIdLockKey :: Text
sapBatchIdLockKey = "SAPReportDispatch:BatchIdCounter:Lock"

getNextBatchId :: (BeamFlow m r, CacheFlow m r) => m Text
getNextBatchId = go (10 :: Int)
  where
    go retriesLeft = do
      mbExisting <- Hedis.get @Integer sapBatchIdCounterKey
      case mbExisting of
        Just _ -> show <$> Hedis.incr sapBatchIdCounterKey
        Nothing -> do
          mbResult <- Hedis.whenWithLockRedisAndReturnValue sapBatchIdLockKey 10 $ do
            mbExisting' <- Hedis.get @Integer sapBatchIdCounterKey
            case mbExisting' of
              Just _ -> Hedis.incr sapBatchIdCounterKey
              Nothing -> do
                mbLatestBatchId <- QSJE.findLatestBatchId
                case mbLatestBatchId >>= (readMaybe . T.unpack) of
                  Just (dbMax :: Integer) -> do
                    void $ Hedis.set sapBatchIdCounterKey dbMax
                    Hedis.incr sapBatchIdCounterKey
                  Nothing -> Hedis.incr sapBatchIdCounterKey
          case mbResult of
            Right val -> pure $ show val
            Left ()
              | retriesLeft > 0 -> do
                threadDelay 1000000
                go (retriesLeft - 1)
              | otherwise ->
                throwError $ InternalError "Failed to acquire SAP batch ID counter lock after retries"

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
  let reqDate = show (utctDay now)
      reqTime = show (utctDayTime now)
      postingDate = show (utctDay fromTime)
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
            headerdesc = entryDescription entryType,
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
-- Single item helper
-- ---------------------------------------------------------------------------

mkItem ::
  (MonadFlow m) =>
  Text ->
  Text ->
  Text ->
  M.Map Text SAPConfig.SAPAccountConfig ->
  PostingDirection ->
  HighPrecMoney ->
  Text ->
  m SAPJournalItem
mkItem bId itemNum acctKey acctMap direction amount currency = do
  acct <- M.lookup acctKey acctMap & fromMaybeM (InternalError $ "SAP account config missing for: " <> acctKey)
  pure
    SAPJournalItem
      { batchId = bId,
        batchItem = itemNum,
        itemdesc = acctKey,
        hkont = acct.hkont,
        amount = show amount,
        shkzg = toShkzg direction,
        kostl = toKostl direction (Just acct),
        prctr = toPrctr direction (Just acct),
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
  let reqDate = show (utctDay now)
      reqTime = show (utctDayTime now)
      postingDate = show (utctDay fromTime)
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
            headerdesc = entryDescription entryType,
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
  UTCTime ->
  UTCTime ->
  Maybe Text ->
  m ()
saveSapJournalEntries req mbResp entryStatus txnType txnCount mId mocid periodStart periodEnd mbErrMsg = do
  now <- getCurrentTime
  let reqHeaders = req.headers
      respHeaders = maybe [] (.responseHeaders) mbResp
  forM_ reqHeaders $ \reqHeader -> do
    entryId <- generateGUID
    let mbRespHeader = find (\rh -> rh.batchId == Just reqHeader.batchId) respHeaders
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
          periodStartTime = periodStart,
          periodEndTime = periodEnd,
          rawResponse = (.rawXml) <$> mbResp,
          merchantId = mId,
          merchantOperatingCityId = mocid,
          createdAt = now,
          updatedAt = now,
          createdBy = "System"
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

sapTokenCacheKey :: Text
sapTokenCacheKey = "SAP:CachedToken"

fetchSAPTokenWithRetry ::
  ( EncFlow m r,
    CacheFlow m r,
    CoreMetrics m,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  SAPConfig.SAPServiceConfig ->
  Int ->
  m (Either Text Text)
fetchSAPTokenWithRetry sapCfg maxRetries = do
  cachedToken <- Hedis.get sapTokenCacheKey
  case (cachedToken :: Maybe Text) of
    Just token -> do
      logInfo "Using cached SAP token"
      pure $ Right token
    Nothing -> go 0
  where
    go attempt = do
      result <- try @_ @SomeException $ SAP.fetchSAPToken sapCfg
      case result of
        Right resp -> do
          Hedis.setExp sapTokenCacheKey resp.access_token resp.expires_in
          pure $ Right resp.access_token
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

hasErrorResponse :: SAPJournalResponse -> Bool
hasErrorResponse resp = any (\hdr -> hdr.msgtyp == Just "E") resp.responseHeaders

handleSAPResponse ::
  (BeamFlow m r, MonadFlow m) =>
  Text ->
  SAPJournalRequest ->
  Either Text SAPJournalResponse ->
  SJE.TransactionType ->
  Int ->
  Text ->
  Text ->
  UTCTime ->
  UTCTime ->
  m Bool
handleSAPResponse label req result txnType txnCount mId mocid periodStart periodEnd =
  case result of
    Left err -> do
      logError $ "SAP " <> label <> " dispatch failed: " <> err
      saveSapJournalEntries req Nothing SJE.FAILED txnType txnCount mId mocid periodStart periodEnd (Just err)
      pure False
    Right resp
      | hasErrorResponse resp -> do
        logError $ "SAP " <> label <> " dispatch returned error response"
        saveSapJournalEntries req (Just resp) SJE.FAILED txnType txnCount mId mocid periodStart periodEnd (Just "SAP returned error msgtyp=E")
        pure False
      | otherwise -> do
        forM_ resp.responseHeaders $ \hdr ->
          logInfo $ "SAP " <> label <> " response: batchId=" <> fromMaybe "" hdr.batchId <> " msgtyp=" <> fromMaybe "" hdr.msgtyp
        saveSapJournalEntries req (Just resp) SJE.SUCCESS txnType txnCount mId mocid periodStart periodEnd Nothing
        pure True
