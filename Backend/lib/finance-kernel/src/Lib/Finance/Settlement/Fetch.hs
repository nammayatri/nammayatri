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
  let mbParserMap = Just parserTypeMap
   in case sourceConfig of
        EmailSourceConfig emailCfg -> do
          eCsv <- EmailSource.fetchSettlementFile emailCfg
          pure $ case eCsv of
            Left err -> Left err
            Right bs ->
              let ty = resolveSplitCustomerType mbParserMap (fromMaybe "" emailCfg.subjectFilter)
               in Right (bs, Nothing, Just ty)
        SFTPSourceConfig sftpCfg _unusedFragmentFromKernel -> do
          eCsv <-
            SFTPSource.fetchSettlementFile
              merchantId
              merchantOperatingCityId
              (settlementServiceToPaymentGatewayName settlementService)
              mbParserMap
              sftpCfg
          pure $ case eCsv of
            Left err -> Left err
            Right (bs, meta, splitTy) -> Right (bs, Just meta, Just splitTy)
