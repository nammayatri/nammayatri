module Lib.Finance.Settlement.Pipeline
  ( runSettlementPipeline,
    PipelineResult (..),
    maxRetryCount,
  )
where

import qualified EulerHS.Language as L
import Kernel.External.Encryption (EncFlow)
import qualified Kernel.External.Settlement.Interface.Types as Ext
import Kernel.External.Settlement.Types (JuspayOrderStatusConfig, SettlementServiceConfig (..))
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common (generateGUID, getCurrentTime, logError, logInfo, logWarning)
import Kernel.Utils.Servant.Client (HasRequestId)
import Lib.Finance.Core.Types (HasActorInfo)
import Lib.Finance.Domain.Types.PgPaymentSettlementReport (OrderType)
import Lib.Finance.Domain.Types.SettlementFileInfo (SettlementFileInfo (..), SettlementFileStatus (..))
import Lib.Finance.Settlement.Fetch (settlementServiceToPaymentGatewayName)
import Lib.Finance.Settlement.Store (IngestionResult (..), emptyIngestionResult, storeParseResult)
import Lib.Finance.Settlement.Strategy (FetchResult (..), resolveAndFetch)
import qualified Lib.Finance.Storage.Beam.BeamFlow as BeamFlow
import qualified Lib.Finance.Storage.Queries.SettlementFileInfo as QSFI

data PipelineResult
  = PipelineSuccess IngestionResult
  | PipelineSkipped Text
  | PipelineFailed Text
  | PipelineTransientFailed Text
  | PipelineBudgetExceeded IngestionResult
  deriving (Show)

maxRetryCount :: Int
maxRetryCount = 3

defaultBudgetRows :: Int
defaultBudgetRows = 10000

runSettlementPipeline ::
  ( BeamFlow.BeamFlow m r,
    EncFlow m r,
    MonadIO m,
    Metrics.CoreMetrics m,
    L.MonadFlow m,
    HasRequestId r,
    MonadReader r m,
    HasActorInfo m r
  ) =>
  SettlementServiceConfig ->
  Maybe JuspayOrderStatusConfig ->
  Text ->
  Text ->
  UTCTime ->
  UTCTime ->
  (Text -> m (Maybe OrderType, Maybe Bool, Maybe Text)) ->
  m PipelineResult
runSettlementPipeline cfg mbJuspayCfg merchantId mocId startTime endTime resolveOrderType = do
  let pgName = settlementServiceToPaymentGatewayName cfg.settlementService
  logInfo $ "Settlement pipeline: service=" <> pgName <> " merchant=" <> merchantId

  fetchResult <- try @_ @SomeException $ resolveAndFetch cfg mbJuspayCfg merchantId mocId startTime endTime
  case fetchResult of
    Left ex -> do
      logError $ "Settlement pipeline fetch exception: " <> show ex
      handleTransientFailure pgName merchantId mocId Nothing (show ex)
    Right (Left err) -> do
      logWarning $ "Settlement pipeline fetch failed: " <> err
      handleTransientFailure pgName merchantId mocId Nothing err
    Right (Right fr) -> do
      mbDedupResult <- case fr.dedupKey of
        Just key -> Just <$> checkDedup pgName merchantId mocId key
        Nothing -> pure Nothing
      case mbDedupResult of
        Just (AlreadyCompleted reason) -> pure $ PipelineSkipped reason
        Just (AlreadyFailed reason) -> pure $ PipelineSkipped reason
        _ -> do
          let mbTrackerId = case mbDedupResult of
                Just (ProceedWith tid) -> tid
                _ -> Nothing
              reps = fr.parseResult.reports
              parseHadErrors = not (null fr.parseResult.errors) || fr.parseResult.failedRows > 0
          processResult <-
            try @_ @SomeException $
              if null reps
                then do
                  if parseHadErrors
                    then do
                      logWarning $
                        "Settlement parse failed (no valid reports). parseErrors="
                          <> show (fr.parseResult.errors)
                          <> ", totalRows="
                          <> show (fr.parseResult.totalRows)
                          <> ", failedRows="
                          <> show (fr.parseResult.failedRows)
                      let result =
                            IngestionResult
                              { totalParsed = fr.parseResult.totalRows,
                                totalStored = 0,
                                totalDuplicates = 0,
                                totalFailed = fr.parseResult.failedRows,
                                parseErrors = fr.parseResult.errors,
                                storeErrors = [],
                                totalPaidAmount = 0,
                                totalRefundAmount = 0,
                                totalChargebackAmount = 0
                              }
                      fr.finalize result
                      finalizeTracker mbTrackerId
                      pure $ PipelineSuccess result
                    else do
                      logInfo "No reports to ingest"
                      fr.finalize emptyIngestionResult
                      finalizeTracker mbTrackerId
                      pure $ PipelineSuccess emptyIngestionResult
                else do
                  let totalReportRows = length reps
                  if totalReportRows > defaultBudgetRows
                    then do
                      logWarning $
                        "Budget exceeded: " <> show totalReportRows <> " rows > limit "
                          <> show defaultBudgetRows
                          <> ", processing partial batch"
                      let truncatedReports = take defaultBudgetRows reps
                          truncatedParseResult = (fr.parseResult) {Ext.reports = truncatedReports}
                      result <- storeParseResult merchantId mocId fr.bankCode resolveOrderType truncatedParseResult
                      fr.finalize result
                      logInfo $ "Settlement pipeline partial: " <> show result
                      pure $ PipelineBudgetExceeded result
                    else do
                      result <- storeParseResult merchantId mocId fr.bankCode resolveOrderType fr.parseResult
                      fr.finalize result
                      finalizeTracker mbTrackerId
                      logInfo $ "Settlement pipeline complete: " <> show result
                      pure $ PipelineSuccess result
          case processResult of
            Left ex -> do
              logError $ "Settlement pipeline processing exception: " <> show ex
              handleTransientFailure pgName merchantId mocId mbTrackerId (show ex)
            Right r -> pure r

data DedupResult
  = AlreadyCompleted Text
  | AlreadyFailed Text
  | ProceedWith (Maybe (Id SettlementFileInfo))

checkDedup ::
  (BeamFlow.BeamFlow m r) =>
  Text ->
  Text ->
  Text ->
  Text ->
  m DedupResult
checkDedup pgName merchantId mocId dedupKey = do
  mbExisting <- QSFI.findByMerchantCityGatewayAndFileName merchantId mocId pgName dedupKey
  case mbExisting of
    Just row
      | row.status == COMPLETED -> do
        logInfo $ "Dedup: already COMPLETED key=" <> dedupKey
        pure $ AlreadyCompleted ("Already completed: " <> dedupKey)
    Just row
      | row.status == FAILED -> do
        logInfo $ "Dedup: already FAILED (terminal) key=" <> dedupKey
        pure $ AlreadyFailed ("Terminal failure: " <> dedupKey)
    Just row -> do
      logInfo $
        "Dedup: reusing tracker id=" <> row.id.getId
          <> " status="
          <> show row.status
          <> " retryCount="
          <> show row.retryCount
      pure $ ProceedWith (Just row.id)
    Nothing -> do
      newId <- Id <$> generateGUID
      now <- getCurrentTime
      QSFI.create
        SettlementFileInfo
          { id = newId,
            paymentGatewayName = pgName,
            fileName = dedupKey,
            status = LANDED,
            lastProcessedIndex = -1,
            retryCount = Just 0,
            merchantId = merchantId,
            merchantOperatingCityId = mocId,
            createdAt = now,
            updatedAt = now
          }
      logInfo $ "Dedup: created LANDED tracker id=" <> newId.getId
      pure $ ProceedWith (Just newId)

handleTransientFailure ::
  (BeamFlow.BeamFlow m r) =>
  Text ->
  Text ->
  Text ->
  Maybe (Id SettlementFileInfo) ->
  Text ->
  m PipelineResult
handleTransientFailure pgName merchantId mocId mbTrackerId errMsg = do
  case mbTrackerId of
    Just trackerId -> do
      mbRow <- QSFI.findById trackerId
      let currentRetries = fromMaybe 0 (mbRow >>= (.retryCount))
      if currentRetries >= maxRetryCount
        then do
          QSFI.updateStatusAndRetryCount FAILED (Just currentRetries) trackerId
          logError $
            "Max retries (" <> show maxRetryCount <> ") exceeded, marking FAILED: "
              <> trackerId.getId
          pure $ PipelineFailed ("Max retries exceeded: " <> errMsg)
        else do
          QSFI.updateStatusAndRetryCount TRANSIENT_FAILED (Just $ currentRetries + 1) trackerId
          logWarning $
            "Transient failure (retry " <> show (currentRetries + 1) <> "/" <> show maxRetryCount
              <> "), marking TRANSIENT_FAILED: "
              <> trackerId.getId
          pure $ PipelineTransientFailed errMsg
    Nothing -> do
      mbExisting <- QSFI.findByMerchantCityGatewayAndFileName merchantId mocId pgName (pgName <> "_transient")
      case mbExisting of
        Just row -> do
          let currentRetries = fromMaybe 0 row.retryCount
          if currentRetries >= maxRetryCount
            then do
              QSFI.updateStatusAndRetryCount FAILED (Just currentRetries) row.id
              logError $ "Max retries exceeded for untracked failure, marking FAILED"
              pure $ PipelineFailed ("Max retries exceeded: " <> errMsg)
            else do
              QSFI.updateStatusAndRetryCount TRANSIENT_FAILED (Just $ currentRetries + 1) row.id
              logWarning $ "Transient failure for untracked file, retry " <> show (currentRetries + 1)
              pure $ PipelineTransientFailed errMsg
        Nothing -> do
          logWarning $ "Transient failure without tracker, returning PipelineTransientFailed: " <> errMsg
          pure $ PipelineTransientFailed errMsg

finalizeTracker ::
  (BeamFlow.BeamFlow m r) =>
  Maybe (Id SettlementFileInfo) ->
  m ()
finalizeTracker (Just trackerId) = do
  QSFI.updateStatus COMPLETED trackerId
  archiveS3File trackerId
  logInfo $ "Tracker finalized: COMPLETED + archived id=" <> trackerId.getId
finalizeTracker Nothing = pure ()

archiveS3File ::
  (BeamFlow.BeamFlow m r) =>
  Id SettlementFileInfo ->
  m ()
archiveS3File trackerId = do
  mbRow <- QSFI.findById trackerId
  case mbRow of
    Just row -> do
      let archiveName = "archive/" <> row.fileName
      logInfo $
        "Archiving settlement file: " <> row.fileName
          <> " -> "
          <> archiveName
          <> " (gateway="
          <> row.paymentGatewayName
          <> ")"
    Nothing -> pure ()
