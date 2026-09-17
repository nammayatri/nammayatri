module Lib.Finance.Settlement.Strategy
  ( resolveAndFetch,
    fetchViaApi,
    FetchResult (..),
  )
where

import qualified EulerHS.Language as L
import Kernel.External.Encryption (EncFlow)
import Kernel.External.Settlement.Interface (parseAndEnrichPaymentSettlementCsv)
import Kernel.External.Settlement.Interface.Types (ParsePaymentSettlementResult, ParseResult (..))
import Kernel.External.Settlement.Types (JuspayOrderStatusConfig, SettlementServiceConfig (..), SettlementSourceConfig (..))
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common (logInfo)
import Kernel.Utils.Logging (logDebug)
import Kernel.Utils.Servant.Client (HasRequestId)
import qualified Lib.Finance.Domain.Types.PgSettlementBatch as PSB
import Lib.Finance.Domain.Types.SettlementFileInfo (SettlementFileStatus (..))
import Lib.Finance.Settlement.Fetch (SftpFetchMeta (..), fetchSettlementCsv)
import Lib.Finance.Settlement.Helpers (dayLevelDedupKey)
import Lib.Finance.Settlement.Sources.BillDeskApi (fetchBillDeskSettlementData)
import Lib.Finance.Settlement.Store (IngestionResult (..))
import qualified Lib.Finance.Storage.Beam.BeamFlow as BeamFlow
import qualified Lib.Finance.Storage.Queries.PgSettlementBatch as QPgSB
import qualified Lib.Finance.Storage.Queries.SettlementFileInfo as QSFI

data FetchResult m = FetchResult
  { parseResult :: ParsePaymentSettlementResult,
    bankCode :: Maybe Text,
    dedupKey :: Maybe Text,
    finalize :: IngestionResult -> m ()
  }

resolveAndFetch ::
  ( BeamFlow.BeamFlow m r,
    EncFlow m r,
    MonadIO m,
    Metrics.CoreMetrics m,
    L.MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  SettlementServiceConfig ->
  Maybe JuspayOrderStatusConfig ->
  Text ->
  Text ->
  UTCTime ->
  UTCTime ->
  m (Either Text (FetchResult m))
resolveAndFetch cfg mbJuspayCfg merchantId mocId startTime endTime =
  case cfg.sourceConfig of
    BillDeskApiSourceConfig apiCfg ->
      fetchViaApi
        "BillDeskApi"
        cfg.bankCode
        endTime
        (fetchBillDeskSettlementData apiCfg merchantId mocId startTime endTime)
        (\batchIds result -> when (null result.storeErrors) $ markBatchesCompleted batchIds)
    _csvSource ->
      fetchViaCsv cfg mbJuspayCfg merchantId mocId startTime endTime

fetchViaApi ::
  (BeamFlow.BeamFlow m r, MonadIO m) =>
  Text ->
  Maybe Text ->
  UTCTime ->
  m (ParsePaymentSettlementResult, a) ->
  (a -> IngestionResult -> m ()) ->
  m (Either Text (FetchResult m))
fetchViaApi prefix bankCode dedupTime fetchAction mkFinalize = do
  let key = dayLevelDedupKey prefix dedupTime
  logInfo $ "API strategy: fetching with dedupKey=" <> key
  apiResult <- try @_ @SomeException fetchAction
  case apiResult of
    Left err -> pure $ Left $ "API fetch failed: " <> show err
    Right (parseResult, metadata) ->
      pure $
        Right
          FetchResult
            { parseResult = parseResult,
              bankCode = bankCode,
              dedupKey = Just key,
              finalize = mkFinalize metadata
            }

fetchViaCsv ::
  ( BeamFlow.BeamFlow m r,
    EncFlow m r,
    MonadIO m,
    Metrics.CoreMetrics m,
    L.MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  SettlementServiceConfig ->
  Maybe JuspayOrderStatusConfig ->
  Text ->
  Text ->
  UTCTime ->
  UTCTime ->
  m (Either Text (FetchResult m))
fetchViaCsv cfg mbJuspayCfg merchantId mocId startTime endTime = do
  csvResult <- fetchSettlementCsv cfg merchantId mocId (Just startTime) (Just endTime)
  case csvResult of
    Left err -> pure $ Left err
    Right (csvBytes, mbSftpMeta, mbSplitCustomerTy) -> do
      logDebug $ "CSV strategy: csvBytes=" <> show csvBytes
      logDebug $ "CSV strategy: mbSftpMeta=" <> show mbSftpMeta
      logDebug $ "CSV strategy: mbSplitCustomerTy=" <> show mbSplitCustomerTy
      let sftpDeliveredZeroRows = case mbSftpMeta of
            Just meta -> not meta.atomicPull && meta.dataRowsDelivered == 0
            Nothing -> False
      parseResult <-
        if sftpDeliveredZeroRows
          then do
            logInfo "SFTP delivered 0 data rows past cursor; skipping parse and treating file as complete"
            pure $ ParseResult {reports = [], totalRows = 0, failedRows = 0, errors = []}
          else parseAndEnrichPaymentSettlementCsv cfg mbJuspayCfg mbSplitCustomerTy csvBytes
      pure $
        Right
          FetchResult
            { parseResult = parseResult,
              bankCode = cfg.bankCode,
              dedupKey = Nothing,
              finalize = \result -> finalizeCsvTracking mbSftpMeta (result.totalParsed == 0)
            }

markBatchesCompleted ::
  (BeamFlow.BeamFlow m r) =>
  [Id PSB.PgSettlementBatch] ->
  m ()
markBatchesCompleted batchIds =
  forM_ batchIds $ \bId -> do
    QPgSB.updateIngestionStatus PSB.INGESTED bId
    logInfo $ "Marked batch as INGESTED: " <> bId.getId

finalizeCsvTracking ::
  (BeamFlow.BeamFlow m r) =>
  Maybe SftpFetchMeta ->
  Bool ->
  m ()
finalizeCsvTracking Nothing _ = logInfo "finalizeCsvTracking: no meta → no-op"
finalizeCsvTracking (Just SftpFetchMeta {..}) hadNoReports
  | not atomicPull && not hadNoReports && dataRowsDelivered > 0 = do
    let newIndex = firstDataRowIndex + dataRowsDelivered - 1
    logInfo $
      "finalizeCsvTracking: updateProgress(PENDING) trackedFileId="
        <> show trackedFileId
        <> " newIndex="
        <> show newIndex
    QSFI.updateProgress PENDING newIndex trackedFileId
  | otherwise = do
    logInfo $
      "finalizeCsvTracking: updateStatus(COMPLETED) trackedFileId="
        <> show trackedFileId
    QSFI.updateStatus COMPLETED trackedFileId
