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
import Kernel.Types.Error (GenericError (..))
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common (fromMaybeM, generateGUID, getCurrentTime, logInfo, logWarning)
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
  Maybe UTCTime ->
  Maybe UTCTime ->
  m ParsePaymentSettlementResult
fetchBillDeskSettlementData apiCfg merchantId mocId mbStartTime mbEndTime = do
  startTime <- mbStartTime & fromMaybeM (InternalError "Settlement ingestion requires startTime")
  endTime <- mbEndTime & fromMaybeM (InternalError "Settlement ingestion requires endTime")
  logInfo $ "BillDesk API: fetching settlements fromDate=" <> show startTime <> " toDate=" <> show endTime
  settlements <- getSettlements apiCfg (Just startTime) (Just endTime) Nothing
  logInfo $ "BillDesk API: found " <> show (length settlements) <> " settlement(s)"
  forM_ settlements $ \settlement ->
    storeSettlementBatch "BILLDESK" merchantId mocId settlement
  results <- forM settlements $ \settlement -> do
    logInfo $ "BillDesk API: fetching details for pv_number=" <> settlement.pvNumber
    getSettlementDetailReports apiCfg settlement
  pure $ mergeParseResults results

mergeParseResults :: [ParsePaymentSettlementResult] -> ParsePaymentSettlementResult
mergeParseResults results =
  ParseResult
    { reports = concatMap (.reports) results,
      totalRows = sum $ map (.totalRows) results,
      failedRows = sum $ map (.failedRows) results,
      errors = concatMap (.errors) results
    }

storeSettlementBatch ::
  (BeamFlow.BeamFlow m r) =>
  Text ->
  Text ->
  Text ->
  SettlementSummary ->
  m ()
storeSettlementBatch pgName merchantId mocId settlement = do
  mbExisting <- QPgSB.findByMerchantCityGatewayAndPvNumber merchantId mocId pgName settlement.pvNumber
  case mbExisting of
    Just _ ->
      logInfo $ "Settlement batch already exists for pvNumber=" <> settlement.pvNumber <> ", skipping"
    Nothing -> do
      batchId <- generateGUID
      now <- getCurrentTime
      let batch = convertToSettlementBatch (Id batchId) pgName merchantId mocId now settlement
      result <- try @_ @SomeException $ QPgSB.create batch
      case result of
        Right _ ->
          logInfo $ "Stored settlement batch pvNumber=" <> settlement.pvNumber
        Left err ->
          logWarning $ "Failed to store settlement batch pvNumber=" <> settlement.pvNumber <> ": " <> show err

convertToSettlementBatch ::
  Id PSB.PgSettlementBatch ->
  Text ->
  Text ->
  Text ->
  UTCTime ->
  SettlementSummary ->
  PSB.PgSettlementBatch
convertToSettlementBatch batchId pgName merchantId mocId now settlement =
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
          status = settlement.status,
          settlementDate = settlement.settlementDate,
          utr = settlement.utr,
          utrDate = settlement.utrDate,
          merchantId = merchantId,
          merchantOperatingCityId = mocId,
          createdAt = now,
          updatedAt = now
        }
