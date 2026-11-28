{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Lib.Finance.Settlement.Store
  ( storeParseResult,
    storeParseResultWithFileId,
    IngestionResult (..),
    emptyIngestionResult,
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.List as List
import qualified Data.Text as T
import Kernel.Beam.Functions (ToTType' (..))
import Kernel.External.Settlement.Interface.Types (ParsePaymentSettlementResult, ParseResult (..), TxnType (..))
import qualified Kernel.External.Settlement.Interface.Types as Ext
import Kernel.Prelude
import qualified Kernel.Types.ActorInfo as Fields
import Kernel.Types.Common (HighPrecMoney)
import qualified Kernel.Types.Id as Fields
import Kernel.Utils.Common (logInfo, logWarning)
import Lib.Finance.Audit.Interface (AuditInput (..))
import qualified Lib.Finance.Audit.Service as Audit
import Lib.Finance.Core.Types (ActorInfo, HasActorInfo)
import Lib.Finance.Domain.Types.AuditEntry (AuditAction (..))
import qualified Lib.Finance.Domain.Types.AuditEntry as AuditDomain
import Lib.Finance.Domain.Types.PgPaymentSettlementReport (PgPaymentSettlementReport (..))
import qualified Lib.Finance.Domain.Types.PgPaymentSettlementReport as Dom
import qualified Lib.Finance.Settlement.Transformer as Transformer
import qualified Lib.Finance.Storage.Beam.BeamFlow as BeamFlow
import qualified Lib.Finance.Storage.Beam.PgPaymentSettlementReport as BeamPgReport
import qualified Lib.Finance.Storage.Queries.PgPaymentSettlementReport as QPgReport
import qualified Lib.Finance.Utils.SensitiveData as SD

chunkSize :: Int
chunkSize = 128

data IngestionResult = IngestionResult
  { totalParsed :: Int,
    totalStored :: Int,
    totalDuplicates :: Int,
    totalFailed :: Int,
    parseErrors :: [Text],
    storeErrors :: [Text],
    totalPaidAmount :: HighPrecMoney,
    totalRefundAmount :: HighPrecMoney,
    totalChargebackAmount :: HighPrecMoney
  }
  deriving (Show, Generic, ToJSON)

emptyIngestionResult :: IngestionResult
emptyIngestionResult = IngestionResult 0 0 0 0 [] [] 0 0 0

storeParseResult ::
  (BeamFlow.BeamFlow m r, HasActorInfo m r) =>
  Text ->
  Text ->
  Maybe Text ->
  (Text -> m (Maybe Dom.OrderType, Maybe Bool, Maybe Text)) ->
  ParsePaymentSettlementResult ->
  m IngestionResult
storeParseResult = storeParseResultWithFileId Nothing

storeParseResultWithFileId ::
  (BeamFlow.BeamFlow m r, HasActorInfo m r) =>
  Maybe Text ->
  Text ->
  Text ->
  Maybe Text ->
  (Text -> m (Maybe Dom.OrderType, Maybe Bool, Maybe Text)) ->
  ParsePaymentSettlementResult ->
  m IngestionResult
storeParseResultWithFileId mbFileId merchantId merchantOperatingCityId mbBankCode resolveOrderType parseResult = do
  actorInfo <- asks (.actorInfo)
  logInfo $
    "Parse complete. Total rows: " <> show (totalRows parseResult)
      <> ", Failed: "
      <> show (failedRows parseResult)
      <> ", Errors: "
      <> show (length $ errors parseResult)

  when (not $ null $ errors parseResult) $
    logWarning $ "Parse errors: " <> show (errors parseResult)

  let indexedReports = zip [0 :: Int ..] (reports parseResult)
      chunks = chunksOf chunkSize indexedReports

  chunkResults <- forM chunks $ \chunk -> do
    pgReports <- forM chunk $ \(idx, report) -> do
      orderId' <-
        if report.txnType == CHARGEBACK && T.null report.orderId
          then case report.txnId of
            Just refTxnId -> maybe report.orderId (.orderId) <$> QPgReport.findOrderByTxnId refTxnId
            Nothing -> pure report.orderId
          else pure report.orderId
      let deterministicId = case mbFileId of
            Just fId -> Just $ fId <> "_" <> T.pack (show idx)
            Nothing -> Nothing
      Transformer.toPgPaymentSettlementReportWithId
        deterministicId
        merchantId
        merchantOperatingCityId
        Nothing
        Nothing
        mbBankCode
        resolveOrderType
        (report {Ext.orderId = orderId'})

    results <- forM pgReports $ \pgReport -> do
      result <- try @_ @SomeException $ QPgReport.create pgReport
      case result of
        Right _ -> do
          auditCreate actorInfo pgReport
          pure (True, Nothing, False, pgReport)
        Left err ->
          let errMsg = show err
           in if isUniqueViolation errMsg
                then pure (False, Nothing, True, pgReport)
                else pure (False, Just $ "Store error for orderId " <> pgReport.orderId <> ": " <> T.pack errMsg, False, pgReport)
    pure results

  let allResults = concat chunkResults
      stored = length [() | (True, _, _, _) <- allResults]
      storeErrs = [e | (_, Just e, _, _) <- allResults]
      duplicates = length [() | (_, _, True, _) <- allResults]
      storedReports = [r | (True, _, _, r) <- allResults]
      paidAmt = sum [r.settlementAmount | r <- storedReports, r.txnType == Dom.ORDER]
      refundAmt = sum [fromMaybe 0 r.refundAmount | r <- storedReports, r.txnType == Dom.REFUND]
      chargebackAmt = sum [fromMaybe 0 r.chargebackAmount | r <- storedReports, r.txnType == Dom.CHARGEBACK]

  logInfo $
    "Ingestion complete. Stored: " <> show stored
      <> ", Duplicates: "
      <> show duplicates
      <> ", Store errors: "
      <> show (length storeErrs)
      <> ", Paid: "
      <> show paidAmt
      <> ", Refunds: "
      <> show refundAmt
      <> ", Chargebacks: "
      <> show chargebackAmt

  pure
    IngestionResult
      { totalParsed = totalRows parseResult,
        totalStored = stored,
        totalDuplicates = duplicates,
        totalFailed = failedRows parseResult + length storeErrs,
        parseErrors = errors parseResult,
        storeErrors = storeErrs,
        totalPaidAmount = paidAmt,
        totalRefundAmount = refundAmt,
        totalChargebackAmount = chargebackAmt
      }

isUniqueViolation :: String -> Bool
isUniqueViolation msg =
  "unique" `List.isInfixOf` map toLower msg
    || "duplicate key" `List.isInfixOf` map toLower msg
    || "already exists" `List.isInfixOf` map toLower msg
  where
    toLower c
      | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
      | otherwise = c

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = let (chunk, rest) = splitAt n xs in chunk : chunksOf n rest

auditCreate ::
  BeamFlow.BeamFlow m r =>
  ActorInfo ->
  PgPaymentSettlementReport ->
  m ()
auditCreate actorInfo report = do
  auditResult <-
    Audit.logAudit
      AuditInput
        { entityType = AuditDomain.PgPaymentSettlementReport,
          entityId = report.id.getId,
          action = Created,
          actorType = actorInfo.actorType,
          actorId = actorInfo.actorId,
          beforeState = Nothing,
          afterState = Just $ toAuditValue report,
          merchantId = report.merchantId,
          merchantOperatingCityId = report.merchantOperatingCityId
        }
  case auditResult of
    Left err ->
      logWarning $
        "Failed to audit PgPaymentSettlementReport (Created): " <> show err
    Right _ -> pure ()

toAuditValue :: PgPaymentSettlementReport -> Aeson.Value
toAuditValue =
  Aeson.toJSON . toTType' @BeamPgReport.PgPaymentSettlementReport . maskSensitive
  where
    maskSensitive :: PgPaymentSettlementReport -> PgPaymentSettlementReport
    maskSensitive PgPaymentSettlementReport {..} =
      PgPaymentSettlementReport
        { cardNumber = SD.maskCardNumber <$> cardNumber,
          rawData = Nothing,
          ..
        }
