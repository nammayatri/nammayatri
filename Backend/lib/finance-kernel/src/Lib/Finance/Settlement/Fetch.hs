module Lib.Finance.Settlement.Fetch
  ( fetchSettlementCsv,
    settlementServiceToPaymentGatewayName,
    SftpFetchMeta (..),
  )
where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Data.Time (defaultTimeLocale, formatTime)
import qualified EulerHS.Language as L
import Kernel.External.Encryption
import Kernel.External.Settlement.Types (SettlementService (..), SettlementServiceConfig (..), SettlementSourceConfig (..), SplitSettlementCustomerType)
import qualified Kernel.External.Settlement.Types as Fields
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common (generateGUID, getCurrentTime, logInfo)
import Kernel.Utils.Servant.Client (HasRequestId)
import Lib.Finance.Domain.Types.SettlementFileInfo (SettlementFileInfo (..), SettlementFileStatus (..))
import Lib.Finance.Settlement.ParserTypeMap (resolveSplitCustomerType)
import qualified Lib.Finance.Settlement.Sources.Email as EmailSource
import qualified Lib.Finance.Settlement.Sources.JuspayApi as JuspayApiSource
import Lib.Finance.Settlement.Sources.SFTP (SftpFetchMeta (..))
import qualified Lib.Finance.Settlement.Sources.SFTP as SFTPSource
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Finance.Storage.Queries.SettlementFileInfo as QSFI

settlementServiceToPaymentGatewayName :: SettlementService -> Text
settlementServiceToPaymentGatewayName = T.pack . show

fetchSettlementCsv ::
  ( BeamFlow m r,
    EncFlow m r,
    MonadIO m,
    CoreMetrics m,
    L.MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  SettlementServiceConfig ->
  Text ->
  Text ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  m (Either Text (LBS.ByteString, Maybe SftpFetchMeta, Maybe SplitSettlementCustomerType))
fetchSettlementCsv SettlementServiceConfig {..} merchantId merchantOperatingCityId mbStartTime mbEndTime =
  case sourceConfig of
    EmailSourceConfig emailCfg -> do
      eCsv <- EmailSource.fetchSettlementFile emailCfg
      case eCsv of
        Left err -> pure $ Left err
        Right bs -> do
          now <- getCurrentTime
          let pgName = settlementServiceToPaymentGatewayName settlementService
              dayLabel = T.pack $ formatTime defaultTimeLocale "%Y-%m-%d" now
              trackerFileName = "email_" <> pgName <> "_" <> dayLabel <> ".csv"
              ty = resolveSplitCustomerType parserTypeMap (fromMaybe "" emailCfg.subjectFilter)
          mbExisting <- QSFI.findByMerchantCityGatewayAndFileName merchantId merchantOperatingCityId pgName trackerFileName
          meta <- case mbExisting of
            Just row | row.status == COMPLETED -> pure Nothing
            Just row -> pure $ Just $ SftpFetchMeta row.id 0 0 True
            Nothing -> do
              newId <- Id <$> generateGUID
              let row =
                    SettlementFileInfo
                      { id = newId,
                        paymentGatewayName = pgName,
                        fileName = trackerFileName,
                        status = LANDED,
                        lastProcessedIndex = -1,
                        retryCount = Just 0,
                        merchantId,
                        merchantOperatingCityId,
                        createdAt = now,
                        updatedAt = now
                      }
              QSFI.create row
              logInfo $ "Email: created LANDED tracker id=" <> newId.getId <> " fileName=" <> trackerFileName
              pure $ Just $ SftpFetchMeta newId 0 0 True
          pure $ Right (bs, meta, Just ty)
    SFTPSourceConfig sftpCfg _unusedFragmentFromKernel -> do
      eCsv <-
        SFTPSource.fetchSettlementFile
          merchantId
          merchantOperatingCityId
          (settlementServiceToPaymentGatewayName settlementService)
          parserTypeMap
          sftpCfg
      pure $ case eCsv of
        Left err -> Left err
        Right (bs, meta, splitTy) -> Right (bs, Just meta, Just splitTy)
    -- Portal-API pull returns a full IST-day export in one call. Dedup is
    -- day-level via a synthetic settlement_file_info row (fileName
    -- juspay_portal_YYYY-MM-DD.csv): the source returns @Nothing@ meta and
    -- empty bytes when the day is already COMPLETED, otherwise @Just meta@
    -- so the ingestion pipeline finalizes the tracker to COMPLETED post-store.
    -- No split-customer-type mapping (the portal parser dispatches on source,
    -- not parserTypeMap).
    JuspayApiSourceConfig apiCfg -> do
      eCsv <-
        JuspayApiSource.fetchSettlementFile
          merchantId
          merchantOperatingCityId
          (settlementServiceToPaymentGatewayName settlementService)
          apiCfg
          mbStartTime
          mbEndTime
      pure $ case eCsv of
        Left err -> Left err
        Right (bs, meta) -> Right (bs, meta, Nothing)
    -- CsvFileSourceConfig: wired via CsvUploadSource once shared-kernel
    -- adds CsvFileSourceConfig_ to SettlementSourceConfig
    BillDeskApiSourceConfig _ ->
      pure $ Left "BillDeskApiSourceConfig is handled by Settlement-API-Strategy, not fetchSettlementCsv"
