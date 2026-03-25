{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Lib.Finance.Settlement.Ingestion
  ( ingestPaymentSettlementReport,
    IngestionResult (..),
  )
where

import Kernel.External.Encryption (EncFlow)
import Kernel.External.Settlement.Interface (fetchAndParsePaymentSettlement)
import Kernel.External.Settlement.Interface.Types (ParseResult (..), PaymentSettlementReport)
import Kernel.External.Settlement.Types (SettlementService, SettlementSourceConfig)
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
  (BeamFlow.BeamFlow m r, EncFlow m r, MonadIO m) =>
  SettlementSourceConfig ->
  SettlementService ->
  Text ->
  Text ->
  m IngestionResult
ingestPaymentSettlementReport sourceConfig service merchantId merchantOperatingCityId = do
  logInfo $ "Starting settlement report ingestion for merchant: " <> merchantId
  fetchResult <- fetchAndParsePaymentSettlement sourceConfig service
  case fetchResult of
    Left err -> do
      logWarning $ "Failed to fetch/parse settlement CSV: " <> err
      pure
        IngestionResult
          { totalParsed = 0,
            totalStored = 0,
            totalDuplicates = 0,
            totalFailed = 1,
            parseErrors = [err],
            storeErrors = []
          }
    Right parseResult -> storeParseResult merchantId merchantOperatingCityId parseResult

storeParseResult ::
  (BeamFlow.BeamFlow m r) =>
  Text ->
  Text ->
  ParseResult PaymentSettlementReport ->
  m IngestionResult
storeParseResult merchantId merchantOperatingCityId parseResult = do
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
