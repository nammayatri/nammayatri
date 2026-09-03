module Lib.Finance.Settlement.Sources.BillDeskApi
  ( fetchBillDeskSettlementData,
  )
where

import Control.Applicative ((<|>))
import qualified Data.Text as T
import Data.Time (TimeZone (..), addDays, defaultTimeLocale, formatTime, localDay, parseTimeM, utcToLocalTime)
import qualified EulerHS.Language as L
import Kernel.External.Encryption (EncFlow)
import Kernel.External.Settlement.BillDesk.ApiTypes (SettlementObj (..))
import Kernel.External.Settlement.Interface (fetchBillDeskSettlementDetails, fetchBillDeskSettlements)
import Kernel.External.Settlement.Interface.Types (ParsePaymentSettlementResult, ParseResult (..))
import Kernel.External.Settlement.Types (BillDeskApiConfig)
import Kernel.External.Settlement.Utils.ParserUtils (parseAmount)
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Common (HighPrecMoney)
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
  Maybe UTCTime ->
  Maybe UTCTime ->
  m ParsePaymentSettlementResult
fetchBillDeskSettlementData apiCfg merchantId mocId mbStartTime mbEndTime = do
  now <- getCurrentTime
  let (fromDateStr, toDateStr) = computeDateRange mbStartTime mbEndTime now
  logInfo $ "BillDesk API: fetching settlements fromDate=" <> fromDateStr <> " toDate=" <> toDateStr
  settlements <- fetchBillDeskSettlements apiCfg (Just fromDateStr) (Just toDateStr) Nothing
  logInfo $ "BillDesk API: found " <> show (length settlements) <> " settlement(s)"
  forM_ settlements $ \settlement ->
    storeSettlementBatch "BILLDESK" merchantId mocId settlement
  results <- forM settlements $ \settlement -> do
    logInfo $ "BillDesk API: fetching details for pv_number=" <> settlement.pv_number
    fetchBillDeskSettlementDetails apiCfg settlement
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
  SettlementObj ->
  m ()
storeSettlementBatch pgName merchantId mocId settlement = do
  mbExisting <- QPgSB.findByMerchantCityGatewayAndPvNumber merchantId mocId pgName settlement.pv_number
  case mbExisting of
    Just _ ->
      logInfo $ "Settlement batch already exists for pvNumber=" <> settlement.pv_number <> ", skipping"
    Nothing -> do
      batchId <- generateGUID
      now <- getCurrentTime
      let batch = convertToSettlementBatch (Id batchId) pgName merchantId mocId now settlement
      result <- try @_ @SomeException $ QPgSB.create batch
      case result of
        Right _ ->
          logInfo $ "Stored settlement batch pvNumber=" <> settlement.pv_number
        Left err ->
          logWarning $ "Failed to store settlement batch pvNumber=" <> settlement.pv_number <> ": " <> show err

convertToSettlementBatch ::
  Id PSB.PgSettlementBatch ->
  Text ->
  Text ->
  Text ->
  UTCTime ->
  SettlementObj ->
  PSB.PgSettlementBatch
convertToSettlementBatch batchId pgName merchantId mocId now settlement =
  let mbAmounts = settlement.amount_details
   in PSB.PgSettlementBatch
        { id = batchId,
          paymentGateway = pgName,
          objectId = settlement.objectid,
          pvNumber = settlement.pv_number,
          mercId = settlement.mercid,
          payoutMercId = settlement.payout_mercid,
          pvFile = settlement.pv_file,
          pvFileDate = settlement.pv_file_date,
          currency = settlement.currency >>= readMaybe . T.unpack,
          settlementAmount = parseAmountMaybe =<< (mbAmounts >>= (.settlement)),
          refundAmount = parseAmountMaybe =<< (mbAmounts >>= (.refund)),
          chargebackAmount = parseAmountMaybe =<< (mbAmounts >>= (.chargeback)),
          refundReversalAmount = parseAmountMaybe =<< (mbAmounts >>= (.refund_reversal)),
          chargebackReversalAmount = parseAmountMaybe =<< (mbAmounts >>= (.chargeback_reversal)),
          adjustmentAmount = parseAmountMaybe =<< (mbAmounts >>= (.adjustment)),
          charges = settlement.charges >>= parseAmountMaybe,
          taxes = settlement.taxes >>= parseAmountMaybe,
          otherAdjustments = settlement.other_adjustments >>= parseAmountMaybe,
          payoutAmount = settlement.payout_amount >>= parseAmountMaybe,
          status = settlement.status,
          settlementDate = settlement.settlement_date >>= parseDateText,
          utr = settlement.utr,
          utrDate = settlement.utr_date >>= parseDateText,
          merchantId = merchantId,
          merchantOperatingCityId = mocId,
          createdAt = now,
          updatedAt = now
        }

parseAmountMaybe :: Text -> Maybe HighPrecMoney
parseAmountMaybe t =
  let stripped = T.strip t
   in if T.null stripped then Nothing else Just (parseAmount stripped)

parseDateText :: Text -> Maybe UTCTime
parseDateText t =
  parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%z" (T.unpack t)
    <|> parseTimeM True defaultTimeLocale "%Y-%m-%d" (T.unpack t)
    <|> parseTimeM True defaultTimeLocale "%d-%m-%Y" (T.unpack t)

computeDateRange :: Maybe UTCTime -> Maybe UTCTime -> UTCTime -> (Text, Text)
computeDateRange (Just startUtc) (Just endUtc) _ =
  let ist = TimeZone 330 False "IST"
      fmtIst = T.pack . formatTime defaultTimeLocale "%Y%m%d" . utcToLocalTime ist
   in (fmtIst startUtc, fmtIst endUtc)
computeDateRange _ _ now =
  let ist = TimeZone 330 False "IST"
      todayIst = localDay (utcToLocalTime ist now)
      yesterdayIst = addDays (-1) todayIst
      fmt = T.pack . formatTime defaultTimeLocale "%Y%m%d"
   in (fmt yesterdayIst, fmt yesterdayIst)
