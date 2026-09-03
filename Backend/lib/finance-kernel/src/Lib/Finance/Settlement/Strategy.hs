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
import Kernel.Utils.Common (getCurrentTime, logInfo)
import Kernel.Utils.Logging (logDebug)
import Kernel.Utils.Servant.Client (HasRequestId)
import Lib.Finance.Domain.Types.SettlementFileInfo (SettlementFileStatus (..))
import Lib.Finance.Settlement.Fetch (SftpFetchMeta (..), fetchSettlementCsv)
import Lib.Finance.Settlement.Helpers (dayLevelDedupKey)
import Lib.Finance.Settlement.Sources.BillDeskApi (fetchBillDeskSettlementData)
import qualified Lib.Finance.Storage.Beam.BeamFlow as BeamFlow
import qualified Lib.Finance.Storage.Queries.SettlementFileInfo as QSFI

data FetchResult m = FetchResult
  { parseResult :: ParsePaymentSettlementResult,
    bankCode :: Maybe Text,
    dedupKey :: Maybe Text,
    finalize :: Bool -> m ()
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
  Maybe UTCTime ->
  Maybe UTCTime ->
  m (Either Text (FetchResult m))
resolveAndFetch cfg mbJuspayCfg merchantId mocId mbStartTime mbEndTime =
  case cfg.sourceConfig of
    BillDeskApiSourceConfig apiCfg ->
      fetchViaApi "BillDeskApi" cfg.bankCode $
        fetchBillDeskSettlementData apiCfg merchantId mocId mbStartTime mbEndTime
    CCAvenueApiSourceConfig _apiCfg ->
      pure $ Left "CCAvenueApi strategy not yet available"
    _csvSource ->
      fetchViaCsv cfg mbJuspayCfg merchantId mocId

fetchViaApi ::
  (BeamFlow.BeamFlow m r, MonadIO m) =>
  Text ->
  Maybe Text ->
  m ParsePaymentSettlementResult ->
  m (Either Text (FetchResult m))
fetchViaApi prefix bankCode fetchAction = do
  now <- getCurrentTime
  let key = dayLevelDedupKey prefix now
  logInfo $ "API strategy: fetching with dedupKey=" <> key
  apiResult <- try @_ @SomeException fetchAction
  case apiResult of
    Left err -> pure $ Left $ "API fetch failed: " <> show err
    Right parseResult ->
      pure $
        Right
          FetchResult
            { parseResult = parseResult,
              bankCode = bankCode,
              dedupKey = Just key,
              finalize = \_ -> pure ()
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
  m (Either Text (FetchResult m))
fetchViaCsv cfg mbJuspayCfg merchantId mocId = do
  csvResult <- fetchSettlementCsv cfg merchantId mocId
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
              finalize = \hadNoReports -> finalizeCsvTracking mbSftpMeta hadNoReports
            }

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
