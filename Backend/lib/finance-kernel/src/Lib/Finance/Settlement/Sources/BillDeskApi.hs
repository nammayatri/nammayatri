module Lib.Finance.Settlement.Sources.BillDeskApi
  ( fetchBillDeskSettlementData,
  )
where

import qualified EulerHS.Language as L
import Kernel.External.Encryption (EncFlow)
import Kernel.External.Settlement.Interface (getSettlementDetailReports, getSettlements)
import Kernel.External.Settlement.Interface.Types (ParsePaymentSettlementResult, ParseResult (..), SettlementSummary (..))
import Kernel.External.Settlement.Types (BillDeskApiConfig)
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common (generateGUID, getCurrentTime, logInfo, logWarning)
import Kernel.Utils.Servant.Client (HasRequestId)
import qualified Lib.Finance.Domain.Types.PgSettlementBatch as PSB
import qualified Lib.Finance.Storage.Beam.BeamFlow as BeamFlow
import qualified Lib.Finance.Storage.Queries.PgSettlementBatch as QPgSB

fetchBillDeskSettlementData ::
  ( BeamFlow.BeamFlow m r,
    EncFlow m r,
    MonadIO m,
    Metrics.CoreMetrics m,
    L.MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  BillDeskApiConfig ->
  Text ->
  Text ->
  UTCTime ->
  UTCTime ->
  m (ParsePaymentSettlementResult, [Id PSB.PgSettlementBatch])
fetchBillDeskSettlementData apiCfg merchantId mocId startTime endTime = do
  logInfo $ "BillDesk API: fetching settlements fromDate=" <> show startTime <> " toDate=" <> show endTime
  settlements <- getSettlements apiCfg (Just startTime) (Just endTime) Nothing
  logInfo $ "BillDesk API: found " <> show (length settlements) <> " settlement(s)"
  batchResults <- forM settlements $ \settlement ->
    storeOrUpdateSettlementBatch "BILLDESK" merchantId mocId settlement
  let pendingBatches = [(s, bId) | (s, (Just bId, False)) <- zip settlements batchResults]
      processedPending = filter (\(s, _) -> deriveSettlementBatchStatus s == PSB.PROCESSED) pendingBatches
  logInfo $
    "BillDesk API: " <> show (length pendingBatches) <> " pending ingestion, "
      <> show (length processedPending)
      <> " with UTR (will fetch details), "
      <> show (length settlements - length pendingBatches)
      <> " already completed"
  results <- forM processedPending $ \(settlement, _) -> do
    logInfo $ "BillDesk API: fetching details for pv_number=" <> settlement.pvNumber
    getSettlementDetailReports apiCfg settlement
  pure (mergeParseResults results, map snd processedPending)

mergeParseResults :: [ParsePaymentSettlementResult] -> ParsePaymentSettlementResult
mergeParseResults results =
  ParseResult
    { reports = concatMap (.reports) results,
      totalRows = sum $ map (.totalRows) results,
      failedRows = sum $ map (.failedRows) results,
      errors = concatMap (.errors) results
    }

deriveSettlementBatchStatus :: SettlementSummary -> PSB.SettlementBatchStatus
deriveSettlementBatchStatus settlement
  | isJust settlement.utr && settlement.status == Just "PROCESSED" = PSB.PROCESSED
  | settlement.status == Just "REJECTED" = PSB.REJECTED
  | otherwise = PSB.PENDING

storeOrUpdateSettlementBatch ::
  (BeamFlow.BeamFlow m r) =>
  Text ->
  Text ->
  Text ->
  SettlementSummary ->
  m (Maybe (Id PSB.PgSettlementBatch), Bool)
storeOrUpdateSettlementBatch pgName merchantId mocId settlement = do
  let batchStatus = deriveSettlementBatchStatus settlement
  mbExisting <- QPgSB.findByMerchantCityGatewayAndPvNumber merchantId mocId pgName settlement.pvNumber
  case mbExisting of
    Just existing -> do
      let isCompleted = existing.ingestionStatus == PSB.INGESTED
      when isCompleted $
        logInfo $ "Settlement batch already ingested for pvNumber=" <> settlement.pvNumber <> ", skipping"
      when (existing.status == PSB.PENDING && batchStatus /= PSB.PENDING) $
        case batchStatus of
          PSB.PROCESSED -> do
            let mbAmounts = settlement.amountDetails
            QPgSB.updateSettlementBatchOnProcessed
              PSB.PROCESSED
              (mbAmounts >>= (.settlement))
              (mbAmounts >>= (.refund))
              (mbAmounts >>= (.chargeback))
              (mbAmounts >>= (.refundReversal))
              (mbAmounts >>= (.chargebackReversal))
              (mbAmounts >>= (.adjustment))
              settlement.charges
              settlement.taxes
              settlement.otherAdjustments
              settlement.payoutAmount
              settlement.settlementDate
              settlement.utr
              settlement.utrDate
              (Just settlement.currency)
              settlement.payoutMerchantId
              settlement.pvFile
              settlement.pvFileDate
              existing.id
            logInfo $ "Settlement batch PENDING→PROCESSED for pvNumber=" <> settlement.pvNumber
          PSB.REJECTED -> do
            QPgSB.updateStatus PSB.REJECTED existing.id
            logInfo $ "Settlement batch PENDING→REJECTED for pvNumber=" <> settlement.pvNumber
          PSB.PENDING -> pure ()
      pure (Just existing.id, isCompleted)
    Nothing -> do
      batchId <- generateGUID
      now <- getCurrentTime
      let batch = convertToSettlementBatch (Id batchId) pgName merchantId mocId now batchStatus settlement
      result <- try @_ @SomeException $ QPgSB.create batch
      case result of
        Right _ -> do
          logInfo $ "Stored settlement batch pvNumber=" <> settlement.pvNumber <> " status=" <> show batchStatus
          pure (Just (Id batchId), False)
        Left err -> do
          logWarning $ "Failed to store settlement batch pvNumber=" <> settlement.pvNumber <> ": " <> show err
          pure (Nothing, False)

convertToSettlementBatch ::
  Id PSB.PgSettlementBatch ->
  Text ->
  Text ->
  Text ->
  UTCTime ->
  PSB.SettlementBatchStatus ->
  SettlementSummary ->
  PSB.PgSettlementBatch
convertToSettlementBatch batchId pgName merchantId mocId now batchStatus settlement =
  let mbAmounts = settlement.amountDetails
   in PSB.PgSettlementBatch
        { id = batchId,
          paymentGateway = pgName,
          objectId = Nothing,
          pvNumber = settlement.pvNumber,
          mercId = settlement.merchantId,
          payoutMercId = settlement.payoutMerchantId,
          pvFile = settlement.pvFile,
          pvFileDate = settlement.pvFileDate,
          currency = Just settlement.currency,
          settlementAmount = mbAmounts >>= (.settlement),
          refundAmount = mbAmounts >>= (.refund),
          chargebackAmount = mbAmounts >>= (.chargeback),
          refundReversalAmount = mbAmounts >>= (.refundReversal),
          chargebackReversalAmount = mbAmounts >>= (.chargebackReversal),
          adjustmentAmount = mbAmounts >>= (.adjustment),
          charges = settlement.charges,
          taxes = settlement.taxes,
          otherAdjustments = settlement.otherAdjustments,
          payoutAmount = settlement.payoutAmount,
          status = batchStatus,
          settlementDate = settlement.settlementDate,
          utr = settlement.utr,
          utrDate = settlement.utrDate,
          ingestionStatus = PSB.INGESTION_PENDING,
          merchantId = merchantId,
          merchantOperatingCityId = mocId,
          createdAt = now,
          updatedAt = now
        }
