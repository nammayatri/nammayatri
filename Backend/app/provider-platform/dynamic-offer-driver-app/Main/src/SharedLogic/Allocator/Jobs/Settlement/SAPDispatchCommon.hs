module SharedLogic.Allocator.Jobs.Settlement.SAPDispatchCommon
  ( -- * Job constraints / lock / idempotency
    SAPJobConstraints,
    lockTTLSeconds,
    mkIdempotencyKey,
    isAlreadyDispatched,
    markAsDispatched,

    -- * Dispatch job shell (unused by callers yet — optional refactor target)
    SAPDispatchShellCfg (..),
    SAPDispatchJobParams (..),
    runSAPDispatchShell,
    NextSAPDispatchSchedule (..),
    buildNextSAPDispatchJobParams,

    -- * Config / token / API
    getSAPConfig,
    fetchSAPTokenWithRetry,
    callSAPWithRetry,
    handleSAPResponse,

    -- * Journal building
    PostingDirection (..),
    toShkzg,
    toKostl,
    toPrctr,
    getNextBatchId,
    mkItem,
    filterZeroItems,
    parseItemAmount,
    computeDebitCreditTotals,
    assertDebitEqualsCredit,
    buildJournalRequestFromItems,
    saveSapJournalEntries,
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
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified Lib.Finance.Core.Types as Finance
import qualified Lib.Finance.Domain.Types.SapJournalEntry as SJE
import Lib.Finance.SapJournalEntry.Interface (SapJournalEntryInput (..))
import qualified Lib.Finance.SapJournalEntry.Service as SapJournalEntryService
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Finance.Storage.Queries.SapJournalEntry as QSJE
import Lib.Scheduler
import Lib.Scheduler.JobStorageType.DB.Table (SchedulerJobT)
import Storage.ConfigPilot.Config.MerchantServiceConfig (MerchantServiceConfigDimensions (..))
import Tools.Error

-- ---------------------------------------------------------------------------
-- Shared constants / idempotency
-- ---------------------------------------------------------------------------

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
-- Common job constraints (allocator SAP dispatch jobs)
-- ---------------------------------------------------------------------------

type SAPJobConstraints m r c =
  ( BeamFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    Finance.HasActorInfo m r,
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
-- Dispatch job shell (shared control flow for subscription / PG / ride jobs)
-- ---------------------------------------------------------------------------

-- | Per-job string labels. Domain fetch/dispatch stays in the caller callback.
data SAPDispatchShellCfg = SAPDispatchShellCfg
  { lockKeyPrefix :: Text,
    idempotencyJobType :: Text,
    jobLabel :: Text
  }

-- | Fields shared by SAPSubscription / SAPPG / SAPRide job data records.
data SAPDispatchJobParams = SAPDispatchJobParams
  { merchantId :: Id DM.Merchant,
    merchantOperatingCityId :: Id DMOC.MerchantOperatingCity,
    scheduledTime :: TimeOfDay,
    timeDiffFromUtc :: Seconds,
    maxApiRetries :: Int,
    startTime :: UTCTime,
    endTime :: UTCTime,
    scheduleNextJob :: Maybe Bool
  }

-- | Pure next-run window for daily SAP dispatch jobs (no scheduler createJobIn).
-- Callers map 'jobParams' into their concrete *JobData and pass 'scheduleAfter' to JC.createJobIn.
data NextSAPDispatchSchedule = NextSAPDispatchSchedule
  { scheduleAfter :: NominalDiffTime,
    jobParams :: SAPDispatchJobParams
  }

-- | Idempotency → lock → config → token → domain callback → mark → schedule next.
runSAPDispatchShell ::
  (SAPJobConstraints m r c) =>
  Text ->
  SAPDispatchShellCfg ->
  SAPDispatchJobParams ->
  (NextSAPDispatchSchedule -> m ()) ->
  (SAPConfig.SAPServiceConfig -> Text -> SAPDispatchJobParams -> m Bool) ->
  m ExecutionResult
runSAPDispatchShell jobId cfg params scheduleNext runDomain = withLogTag ("JobId-" <> jobId) do
  let merchantId = params.merchantId
      merchantOperatingCityId = params.merchantOperatingCityId
      lockKey = cfg.lockKeyPrefix <> ":" <> merchantId.getId <> ":" <> merchantOperatingCityId.getId
      fromTime = params.startTime
      toTime = params.endTime
      idempotencyKey = mkIdempotencyKey cfg.idempotencyJobType merchantOperatingCityId.getId (show $ utctDay fromTime)
      shouldScheduleNext = fromMaybe True params.scheduleNextJob
      scheduleNext' = do
        jobParams <- buildNextSAPDispatchJobParams merchantId merchantOperatingCityId params.scheduledTime params.timeDiffFromUtc params.maxApiRetries
        logInfo $ "Scheduling next SAP " <> cfg.jobLabel <> "dispatch in " <> show jobParams.scheduleAfter
        scheduleNext jobParams

  dispatched <- isAlreadyDispatched idempotencyKey
  if dispatched
    then do
      logInfo $ "SAP " <> cfg.jobLabel <> " already dispatched for " <> show (utctDay fromTime) <> ", skipping"
      when shouldScheduleNext scheduleNext'
      pure Complete
    else do
      mbResult <- Hedis.whenWithLockRedisAndReturnValue lockKey lockTTLSeconds $ do
        logInfo $
          "Starting SAP "
            <> cfg.jobLabel
            <> " dispatch merchantId="
            <> merchantId.getId
            <> " mocId="
            <> merchantOperatingCityId.getId
            <> " from="
            <> show fromTime
            <> " to="
            <> show toTime

        mbSAPConfig <- getSAPConfig merchantOperatingCityId
        case mbSAPConfig of
          Nothing -> do
            logWarning "No SAP config found in MerchantServiceConfig"
            pure True
          Just sapCfg -> do
            let retries = params.maxApiRetries
            tokenResult <- fetchSAPTokenWithRetry sapCfg retries
            case tokenResult of
              Left err -> do
                logError $ "SAP token fetch failed after " <> show retries <> " retries: " <> err
                pure False
              Right token -> do
                result <-
                  try @_ @SomeException $
                    runDomain sapCfg token params
                case result of
                  Left err -> do
                    logError $ "SAP " <> cfg.jobLabel <> " dispatch failed with exception: " <> show err
                    pure False
                  Right ok -> do
                    when ok $ markAsDispatched idempotencyKey
                    pure ok

      case mbResult of
        Left () -> do
          logWarning $ "SAP " <> cfg.jobLabel <> " dispatch lock contention, will retry: " <> lockKey
          pure Retry
        Right succeeded -> do
          when shouldScheduleNext scheduleNext'
          if succeeded
            then pure Complete
            else do
              logWarning $ "SAP " <> cfg.jobLabel <> " dispatch had failures, scheduling next run anyway"
              pure Complete

-- | Compute tomorrow's run delay and today's IST day window as next job params.
buildNextSAPDispatchJobParams ::
  (MonadFlow m) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  TimeOfDay ->
  Seconds ->
  Int ->
  m NextSAPDispatchSchedule
buildNextSAPDispatchJobParams mId mocid scheduledTime' utcOffset maxRetries = do
  now <- getCurrentTime
  let istOffset = secondsToNominalDiffTime utcOffset
      nowIST = addUTCTime istOffset now
      todayDayIST = utctDay nowIST
      tomorrowDayIST = addDays 1 todayDayIST
      tomorrowRunTime = addUTCTime (negate istOffset) $ UTCTime tomorrowDayIST (timeOfDayToTime scheduledTime')
      scheduleAfter' = diffUTCTime tomorrowRunTime now
      nextStartTime = addUTCTime (negate istOffset) $ UTCTime todayDayIST 0
      nextEndTime = addUTCTime (negate istOffset) $ UTCTime todayDayIST (secondsToDiffTime 86399)
  pure
    NextSAPDispatchSchedule
      { scheduleAfter = scheduleAfter',
        jobParams =
          SAPDispatchJobParams
            { merchantId = mId,
              merchantOperatingCityId = mocid,
              scheduledTime = scheduledTime',
              timeDiffFromUtc = utcOffset,
              maxApiRetries = maxRetries,
              startTime = nextStartTime,
              endTime = nextEndTime,
              scheduleNextJob = Just True
            }
      }

-- ---------------------------------------------------------------------------
-- Config
-- ---------------------------------------------------------------------------

getSAPConfig ::
  (BeamFlow m r, CacheFlow m r, EsqDBFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  m (Maybe SAPConfig.SAPServiceConfig)
getSAPConfig mocid = do
  mbConfig <-
    getOneConfig
      (MerchantServiceConfigDimensions {merchantOperatingCityId = mocid.getId, merchantId = Nothing, serviceName = Just (DMSC.SAPService DMSC.Journal)})
      Nothing
  pure $ case mbConfig of
    Just cfg -> case cfg.serviceConfig of
      DMSC.SAPServiceConfig sapCfg -> Just sapCfg
      _ -> Nothing
    Nothing -> Nothing

-- ---------------------------------------------------------------------------
-- Posting direction / GL item helpers
-- ---------------------------------------------------------------------------

data PostingDirection = Debit | Credit

toShkzg :: PostingDirection -> Text
toShkzg Debit = "S"
toShkzg Credit = "H"

toKostl :: PostingDirection -> Maybe SAPConfig.SAPAccountConfig -> Maybe Text
toKostl Debit mbAcct = mbAcct >>= (.kostl)
toKostl Credit _ = Nothing

toPrctr :: PostingDirection -> Maybe SAPConfig.SAPAccountConfig -> Maybe Text
toPrctr Credit mbAcct = mbAcct >>= (.prctr)
toPrctr Debit _ = Nothing

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

filterZeroItems :: [SAPJournalItem] -> [SAPJournalItem]
filterZeroItems = filter (\item -> parseItemAmount item /= 0)

parseItemAmount :: SAPJournalItem -> HighPrecMoney
parseItemAmount item = fromMaybe 0 (readMaybe (T.unpack item.amount) :: Maybe HighPrecMoney)

computeDebitCreditTotals :: [SAPJournalItem] -> (HighPrecMoney, HighPrecMoney)
computeDebitCreditTotals items =
  let debitTotal = sum [parseItemAmount item | item <- items, item.shkzg == "S"]
      creditTotal = sum [parseItemAmount item | item <- items, item.shkzg == "H"]
   in (debitTotal, creditTotal)

assertDebitEqualsCredit :: (MonadThrow m, Log m) => Text -> Text -> HighPrecMoney -> HighPrecMoney -> m ()
assertDebitEqualsCredit label batchId debit credit =
  when (roundTo2 debit /= roundTo2 credit) $
    throwError
      ( InternalError $
          "SAP "
            <> label
            <> " debit/credit mismatch: debit="
            <> show debit
            <> " credit="
            <> show credit
            <> " batchId="
            <> batchId
      )

roundTo2 :: HighPrecMoney -> HighPrecMoney
roundTo2 x = fromIntegral (round (x * 100) :: Integer) / 100

-- ---------------------------------------------------------------------------
-- Journal request builder
-- ---------------------------------------------------------------------------

-- | Generic JV request from pre-built items (ride-revenue / shared builders).
buildJournalRequestFromItems ::
  (BeamFlow m r, CacheFlow m r) =>
  SAPConfig.SAPServiceConfig ->
  Text ->
  UTCTime ->
  [SAPJournalItem] ->
  m SAPJournalRequest
buildJournalRequestFromItems sapCfg headerDesc fromTime items = do
  now <- getCurrentTime
  let reqDate = formatSAPDate now
      reqTime = formatSAPTime now
      postingDate = formatSAPDate fromTime
      filteredItems = filterZeroItems items
      (debit, credit) = computeDebitCreditTotals filteredItems
  bId <-
    case filteredItems of
      (firstItem : _) -> pure firstItem.batchId
      [] -> getNextBatchId
  assertDebitEqualsCredit headerDesc bId debit credit
  let header =
        SAPJournalHeader
          { msgtyp = Nothing,
            batchId = bId,
            requestDate = reqDate,
            requestTime = reqTime,
            headerdesc = headerDesc,
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
  pure SAPJournalRequest {headers = [header]}

formatSAPDate :: UTCTime -> Text
formatSAPDate utcTime =
  let (y, m, d) = toGregorian (utctDay utcTime)
   in T.pack $ show y <> padTwo m <> padTwo d

formatSAPTime :: UTCTime -> Text
formatSAPTime utcTime =
  let tod = timeToTimeOfDay (utctDayTime utcTime)
   in T.pack $ padTwo (todHour tod) <> padTwo (todMin tod) <> padTwo (floor (todSec tod) :: Int)

padTwo :: Int -> String
padTwo n
  | n < 10 = "0" <> show n
  | otherwise = show n

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
-- Persist + SAP API
-- ---------------------------------------------------------------------------

saveSapJournalEntries ::
  (BeamFlow m r, Finance.HasActorInfo m r) =>
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
  let reqHeaders = req.headers
      respHeaders = maybe [] (.responseHeaders) mbResp
  forM_ reqHeaders $ \reqHeader -> do
    let mbRespHeader = find (\rh -> rh.batchId == Just reqHeader.batchId) respHeaders
    let (totalDebit, totalCredit) = computeDebitCreditTotals reqHeader.items
    SapJournalEntryService.createSapJournalEntry
      SapJournalEntryInput
        { belnr = mbRespHeader >>= (.belnr),
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
          merchantOperatingCityId = mocid
        }

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
  (BeamFlow m r, Finance.HasActorInfo m r) =>
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
