module Lib.Finance.Settlement.Pipeline
  ( runSettlementPipeline,
    PipelineResult (..),
  )
where

import qualified EulerHS.Language as L
import Kernel.External.Encryption (EncFlow)
import Kernel.External.Settlement.Types (JuspayOrderStatusConfig, SettlementServiceConfig (..))
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common (generateGUID, getCurrentTime, logInfo, logWarning)
import Kernel.Utils.Servant.Client (HasRequestId)
import Lib.Finance.Core.Types (HasActorInfo)
import Lib.Finance.Domain.Types.PgPaymentSettlementReport (OrderType)
import Lib.Finance.Domain.Types.SettlementFileInfo (SettlementFileInfo (..), SettlementFileStatus (..))
import Lib.Finance.Settlement.Fetch (settlementServiceToPaymentGatewayName)
import Lib.Finance.Settlement.Store (IngestionResult (..), emptyIngestionResult, storeParseResult)
import Lib.Finance.Settlement.Strategy (resolveAndFetch)
import qualified Lib.Finance.Storage.Beam.BeamFlow as BeamFlow
import qualified Lib.Finance.Storage.Queries.SettlementFileInfo as QSFI

data PipelineResult
  = PipelineSuccess IngestionResult
  | PipelineSkipped Text
  | PipelineFailed Text
  deriving (Show)

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
  Maybe UTCTime ->
  Maybe UTCTime ->
  (Text -> m (Maybe OrderType, Maybe Bool, Maybe Text)) ->
  m PipelineResult
runSettlementPipeline cfg mbJuspayCfg merchantId mocId mbStartTime mbEndTime resolveOrderType = do
  let pgName = settlementServiceToPaymentGatewayName cfg.settlementService
  logInfo $ "Settlement pipeline: service=" <> pgName <> " merchant=" <> merchantId

  fetchResult <- resolveAndFetch cfg mbJuspayCfg merchantId mocId mbStartTime mbEndTime
  case fetchResult of
    Left err -> do
      logWarning $ "Settlement pipeline fetch failed: " <> err
      pure $ PipelineFailed err
    Right fr -> do
      mbDedupResult <- case fr.dedupKey of
        Just key -> Just <$> checkDedup pgName merchantId mocId key
        Nothing -> pure Nothing
      case mbDedupResult of
        Just (AlreadyCompleted reason) -> pure $ PipelineSkipped reason
        _ -> do
          let mbTrackerId = case mbDedupResult of
                Just (ProceedWith tid) -> tid
                _ -> Nothing
              reps = fr.parseResult.reports
              parseHadErrors = not (null fr.parseResult.errors) || fr.parseResult.failedRows > 0
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
                  fr.finalize True
                  finalizeTracker mbTrackerId
                  pure $
                    PipelineSuccess
                      IngestionResult
                        { totalParsed = fr.parseResult.totalRows,
                          totalStored = 0,
                          totalDuplicates = 0,
                          totalFailed = max 1 (fr.parseResult.failedRows + length fr.parseResult.errors),
                          parseErrors = fr.parseResult.errors,
                          storeErrors = []
                        }
                else do
                  logInfo "No reports to ingest"
                  fr.finalize True
                  finalizeTracker mbTrackerId
                  pure $ PipelineSuccess emptyIngestionResult
            else do
              result <- storeParseResult merchantId mocId fr.bankCode resolveOrderType fr.parseResult
              fr.finalize (null reps)
              finalizeTracker mbTrackerId
              logInfo $ "Settlement pipeline complete: " <> show result
              pure $ PipelineSuccess result

data DedupResult
  = AlreadyCompleted Text
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
    Just row -> do
      logInfo $ "Dedup: reusing PENDING tracker id=" <> row.id.getId
      pure $ ProceedWith (Just row.id)
    Nothing -> do
      newId <- Id <$> generateGUID
      now <- getCurrentTime
      QSFI.create
        SettlementFileInfo
          { id = newId,
            paymentGatewayName = pgName,
            fileName = dedupKey,
            status = PENDING,
            lastProcessedIndex = -1,
            merchantId = merchantId,
            merchantOperatingCityId = mocId,
            createdAt = now,
            updatedAt = now
          }
      logInfo $ "Dedup: created PENDING tracker id=" <> newId.getId
      pure $ ProceedWith (Just newId)

finalizeTracker ::
  (BeamFlow.BeamFlow m r) =>
  Maybe (Id SettlementFileInfo) ->
  m ()
finalizeTracker (Just trackerId) = do
  QSFI.updateStatus COMPLETED trackerId
  logInfo $ "Tracker finalized: COMPLETED id=" <> trackerId.getId
finalizeTracker Nothing = pure ()
