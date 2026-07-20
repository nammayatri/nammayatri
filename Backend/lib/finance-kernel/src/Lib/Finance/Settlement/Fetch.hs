module Lib.Finance.Settlement.Fetch
  ( fetchSettlementCsv,
    settlementServiceToPaymentGatewayName,
    SftpFetchMeta (..),
  )
where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified EulerHS.Language as L
import Kernel.External.Encryption
import Kernel.External.Settlement.Types (SettlementService (..), SettlementServiceConfig (..), SettlementSourceConfig (..), SplitSettlementCustomerType)
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Utils.Servant.Client (HasRequestId)
import Lib.Finance.Settlement.ParserTypeMap (resolveSplitCustomerType)
import qualified Lib.Finance.Settlement.Sources.Email as EmailSource
import qualified Lib.Finance.Settlement.Sources.JuspayApi as JuspayApiSource
import Lib.Finance.Settlement.Sources.SFTP (SftpFetchMeta (..))
import qualified Lib.Finance.Settlement.Sources.SFTP as SFTPSource
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)

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
  m (Either Text (LBS.ByteString, Maybe SftpFetchMeta, Maybe SplitSettlementCustomerType))
fetchSettlementCsv SettlementServiceConfig {..} merchantId merchantOperatingCityId =
  case sourceConfig of
    EmailSourceConfig emailCfg -> do
      eCsv <- EmailSource.fetchSettlementFile emailCfg
      pure $ case eCsv of
        Left err -> Left err
        Right bs ->
          let ty = resolveSplitCustomerType parserTypeMap (fromMaybe "" emailCfg.subjectFilter)
           in Right (bs, Nothing, Just ty)
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
      pure $ case eCsv of
        Left err -> Left err
        Right (bs, meta) -> Right (bs, meta, Nothing)
