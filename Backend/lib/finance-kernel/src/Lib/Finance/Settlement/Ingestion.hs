{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Lib.Finance.Settlement.Ingestion
  ( ingestPaymentSettlementReport,
    IngestionResult (..),
  )
where

import qualified Data.Aeson as Aeson
import qualified EulerHS.Language as L
import Kernel.Beam.Functions (ToTType' (..))
import Kernel.External.Encryption (EncFlow)
import Kernel.External.Settlement.Interface (parseAndEnrichPaymentSettlementCsv)
import Kernel.External.Settlement.Interface.Types (ParsePaymentSettlementResult, ParseResult (..))
import Kernel.External.Settlement.Types (JuspayOrderStatusConfig, SettlementServiceConfig (..))
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Utils.Common (logInfo, logWarning)
import Kernel.Utils.Logging (logDebug)
import Kernel.Utils.Servant.Client (HasRequestId)
import Lib.Finance.Audit.Interface (AuditInput (..))
import qualified Lib.Finance.Audit.Service as Audit
import Lib.Finance.Core.Types (ActorInfo, HasActorInfo)
import Lib.Finance.Domain.Types.AuditEntry (AuditAction (..))
import qualified Lib.Finance.Domain.Types.AuditEntry as AuditDomain
import Lib.Finance.Domain.Types.PgPaymentSettlementReport (PgPaymentSettlementReport)
import qualified Lib.Finance.Domain.Types.PgPaymentSettlementReport as Dom
import Lib.Finance.Domain.Types.SettlementFileInfo (SettlementFileStatus (..))
import Lib.Finance.Settlement.Fetch (SftpFetchMeta (..), fetchSettlementCsv)
import qualified Lib.Finance.Settlement.Transformer as Transformer
import qualified Lib.Finance.Storage.Beam.BeamFlow as BeamFlow
import qualified Lib.Finance.Storage.Beam.PgPaymentSettlementReport as BeamPgReport
import qualified Lib.Finance.Storage.Queries.PgPaymentSettlementReport as QPgReport
import qualified Lib.Finance.Storage.Queries.SettlementFileInfo as QSFI

pgPaymentSettlementReportToAuditValue :: PgPaymentSettlementReport -> Aeson.Value
pgPaymentSettlementReportToAuditValue report =
  Aeson.toJSON (toTType' report :: BeamPgReport.PgPaymentSettlementReport)

auditPgPaymentSettlementReportCreate ::
  BeamFlow.BeamFlow m r =>
  ActorInfo ->
  PgPaymentSettlementReport ->
  m ()
auditPgPaymentSettlementReportCreate actorInfo report = do
  auditResult <-
    Audit.logAudit
      AuditInput
        { entityType = AuditDomain.PgPaymentSettlementReport,
          entityId = report.id.getId,
          action = Created,
          actorType = actorInfo.actorType,
          actorId = actorInfo.actorId,
          beforeState = Nothing,
          afterState = Just $ pgPaymentSettlementReportToAuditValue report,
          merchantId = report.merchantId,
          merchantOperatingCityId = report.merchantOperatingCityId
        }
  case auditResult of
    Left err ->
      logWarning $
        "Failed to audit PgPaymentSettlementReport (Created): " <> show err
    Right _ -> pure ()

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
  (Text -> m (Maybe Dom.OrderType, Maybe Bool)) ->
  m IngestionResult
ingestPaymentSettlementReport cfg mbJuspayCfg merchantId merchantOperatingCityId resolveOrderType = do
  logInfo $ "Starting settlement report ingestion for merchant: " <> merchantId
  fetchResult <- fetchSettlementCsv cfg merchantId merchantOperatingCityId
  case fetchResult of
    Left err -> do
      logWarning $ "Failed to fetch settlement CSV: " <> err
      pure
        IngestionResult
          { totalParsed = 0,
            totalStored = 0,
            totalDuplicates = 0,
            totalFailed = 1,
            parseErrors = [err],
            storeErrors = []
          }
    Right (csvBytes, mbSftpMeta, mbSplitCustomerTy) -> do
      logDebug $ "ingestPaymentSettlementReport: csvBytes=" <> show csvBytes
      logDebug $ "ingestPaymentSettlementReport: mbSftpMeta=" <> show mbSftpMeta
      logDebug $ "ingestPaymentSettlementReport: mbSplitCustomerTy=" <> show mbSplitCustomerTy
      let sftpDeliveredZeroRows = case mbSftpMeta of
            Just meta -> meta.dataRowsDelivered == 0
            Nothing -> False
      parseResult <-
        if sftpDeliveredZeroRows
          then do
            logInfo "SFTP delivered 0 data rows past cursor; skipping parse and treating file as complete"
            pure $ ParseResult {reports = [], totalRows = 0, failedRows = 0, errors = []}
          else parseAndEnrichPaymentSettlementCsv cfg mbJuspayCfg mbSplitCustomerTy csvBytes
      let parseHadNoReports = null (reports parseResult)
          parseHadErrors = not (null (errors parseResult)) || failedRows parseResult > 0
      result <-
        if parseHadNoReports
          then
            if parseHadErrors
              then do
                logWarning $
                  "Failed to parse settlement CSV (no rows to ingest). parseErrors="
                    <> show (errors parseResult)
                    <> ", totalRows="
                    <> show (totalRows parseResult)
                    <> ", failedRows="
                    <> show (failedRows parseResult)
                pure
                  IngestionResult
                    { totalParsed = totalRows parseResult,
                      totalStored = 0,
                      totalDuplicates = 0,
                      totalFailed = max 1 (failedRows parseResult + length (errors parseResult)),
                      parseErrors = errors parseResult,
                      storeErrors = []
                    }
              else do
                -- Empty payload from fetch (or CSV with header only). Treat as success: nothing to ingest.
                logInfo "Settlement CSV had no rows to ingest; marking file as complete"
                pure
                  IngestionResult
                    { totalParsed = 0,
                      totalStored = 0,
                      totalDuplicates = 0,
                      totalFailed = 0,
                      parseErrors = [],
                      storeErrors = []
                    }
          else storeParseResult merchantId merchantOperatingCityId cfg.bankCode resolveOrderType parseResult
      finalizeSftpFileCursor mbSftpMeta parseHadNoReports
      pure result

finalizeSftpFileCursor ::
  (BeamFlow.BeamFlow m r) =>
  Maybe SftpFetchMeta ->
  Bool ->
  m ()
finalizeSftpFileCursor mbMeta parseHadNoReports = do
  logInfo $
    "finalizeSftpFileCursor: mbMeta=" <> show mbMeta
      <> " parseHadNoReports="
      <> show parseHadNoReports
  maybe (logInfo "finalizeSftpFileCursor: mbMeta=Nothing → no-op") finalize mbMeta
  where
    finalize meta@SftpFetchMeta {..}
      | not parseHadNoReports && dataRowsDelivered > 0 = do
        let newIndex = firstDataRowIndex + dataRowsDelivered - 1
        logInfo $
          "finalizeSftpFileCursor: branch=updateProgress(PENDING)"
            <> " trackedFileId="
            <> show trackedFileId
            <> " firstDataRowIndex="
            <> show firstDataRowIndex
            <> " dataRowsDelivered="
            <> show dataRowsDelivered
            <> " newLastProcessedIndex="
            <> show newIndex
        QSFI.updateProgress PENDING newIndex trackedFileId
        logInfo $ "finalizeSftpFileCursor: updateProgress done meta=" <> show meta
      | otherwise = do
        logInfo $
          "finalizeSftpFileCursor: branch=updateStatus(COMPLETED)"
            <> " trackedFileId="
            <> show trackedFileId
            <> " parseHadNoReports="
            <> show parseHadNoReports
            <> " dataRowsDelivered="
            <> show dataRowsDelivered
        QSFI.updateStatus COMPLETED trackedFileId
        logInfo $ "finalizeSftpFileCursor: updateStatus done meta=" <> show meta -- here we need to fix even if the parser breaks it marks it as completed

storeParseResult ::
  (BeamFlow.BeamFlow m r, HasActorInfo m r) =>
  Text ->
  Text ->
  Maybe Text ->
  (Text -> m (Maybe Dom.OrderType, Maybe Bool)) ->
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
        auditPgPaymentSettlementReportCreate actorInfo pgReport
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
