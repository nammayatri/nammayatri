{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Lib.Finance.Settlement.Ingestion
  ( ingestPaymentSettlementReport,
    IngestionResult (..),
  )
where

import qualified Data.ByteString.Lazy as LBS
import Kernel.External.Settlement.Interface (parsePaymentSettlementCsv)
import Kernel.External.Settlement.Interface.Types (ParseResult (..))
import Kernel.External.Settlement.Types (SettlementParseConfig (..))
import Kernel.Prelude
import Kernel.Utils.Common (logInfo, logWarning)
import qualified Lib.Finance.Settlement.Transformer as Transformer
import qualified Lib.Finance.Storage.Beam.BeamFlow as BeamFlow
import qualified Lib.Finance.Storage.Queries.PgPaymentSettlementReport as QPgReport

data IngestionResult = IngestionResult
  { totalParsed :: Int,
    totalStored :: Int,
    totalDuplicates :: Int,
    totalFailed :: Int,
    parseErrors :: [Text],
    storeErrors :: [Text]
  }
  deriving (Show, Generic, ToJSON)

ingestPaymentSettlementReport ::
  (BeamFlow.BeamFlow m r) =>
  SettlementParseConfig ->
  LBS.ByteString ->
  m IngestionResult
ingestPaymentSettlementReport config csvData = do
  let merchantId = config.merchantId
      merchantOperatingCityId = config.merchantOperatingCityId
  logInfo $ "Starting settlement report ingestion for merchant: " <> merchantId
  let parseResult = parsePaymentSettlementCsv config csvData
  logInfo $
    "Parse complete. Total rows: " <> show (totalRows parseResult)
      <> ", Failed: "
      <> show (failedRows parseResult)
      <> ", Errors: "
      <> show (length $ errors parseResult)

  when (not $ null $ errors parseResult) $
    logWarning $ "Parse errors: " <> show (errors parseResult)

  results <- forM (reports parseResult) $ \report -> do
    existing <- QPgReport.findByOrderIdAndTxnType report.orderId (Transformer.mapTxnType report.txnType)
    if not (null existing)
      then pure (Nothing, True, Nothing)
      else do
        pgReport <- Transformer.toPgPaymentSettlementReport merchantId merchantOperatingCityId Nothing Nothing report
        result <- try @_ @SomeException $ QPgReport.create pgReport
        case result of
          Right _ -> pure (Just pgReport, False, Nothing)
          Left err -> pure (Nothing, False, Just $ "Store error for orderId " <> report.orderId <> ": " <> show err)

  let stored = length [() | (Just _, _, _) <- results]
      duplicates = length [() | (_, True, _) <- results]
      storeErrs = [e | (_, _, Just e) <- results]

  logInfo $
    "Ingestion complete. Stored: " <> show stored
      <> ", Duplicates: "
      <> show duplicates
      <> ", Store errors: "
      <> show (length storeErrs)

  pure
    IngestionResult
      { totalParsed = totalRows parseResult,
        totalStored = stored,
        totalDuplicates = duplicates,
        totalFailed = failedRows parseResult + length storeErrs,
        parseErrors = errors parseResult,
        storeErrors = storeErrs
      }
