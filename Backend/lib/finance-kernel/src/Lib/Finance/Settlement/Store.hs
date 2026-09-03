{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Lib.Finance.Settlement.Store
  ( storeParseResult,
    IngestionResult (..),
    emptyIngestionResult,
  )
where

import qualified Data.Aeson as Aeson
import Kernel.Beam.Functions (ToTType' (..))
import Kernel.External.Settlement.Interface.Types (ParsePaymentSettlementResult, ParseResult (..))
import Kernel.Prelude
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

data IngestionResult = IngestionResult
  { totalParsed :: Int,
    totalStored :: Int,
    totalDuplicates :: Int,
    totalFailed :: Int,
    parseErrors :: [Text],
    storeErrors :: [Text]
  }
  deriving (Show, Generic, ToJSON)

emptyIngestionResult :: IngestionResult
emptyIngestionResult = IngestionResult 0 0 0 0 [] []

storeParseResult ::
  (BeamFlow.BeamFlow m r, HasActorInfo m r) =>
  Text ->
  Text ->
  Maybe Text ->
  (Text -> m (Maybe Dom.OrderType, Maybe Bool, Maybe Text)) ->
  ParsePaymentSettlementResult ->
  m IngestionResult
storeParseResult merchantId merchantOperatingCityId mbBankCode resolveOrderType parseResult = do
  actorInfo <- asks (.actorInfo)
  logInfo $
    "Parse complete. Total rows: " <> show (totalRows parseResult)
      <> ", Failed: "
      <> show (failedRows parseResult)
      <> ", Errors: "
      <> show (length $ errors parseResult)

  when (not $ null $ errors parseResult) $
    logWarning $ "Parse errors: " <> show (errors parseResult)

  results <- forM (reports parseResult) $ \report -> do
    pgReport <- Transformer.toPgPaymentSettlementReport merchantId merchantOperatingCityId Nothing Nothing mbBankCode resolveOrderType report
    result <- try @_ @SomeException $ QPgReport.create pgReport
    case result of
      Right _ -> do
        auditCreate actorInfo pgReport
        pure (Just pgReport, Nothing)
      Left err -> pure (Nothing, Just $ "Store error for orderId " <> report.orderId <> ": " <> show err)

  let stored = length [() | (Just _, _) <- results]
      storeErrs = [e | (_, Just e) <- results]

  logInfo $
    "Ingestion complete. Stored: " <> show stored
      <> ", Store errors: "
      <> show (length storeErrs)

  pure
    IngestionResult
      { totalParsed = totalRows parseResult,
        totalStored = stored,
        totalDuplicates = 0,
        totalFailed = failedRows parseResult + length storeErrs,
        parseErrors = errors parseResult,
        storeErrors = storeErrs
      }

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
