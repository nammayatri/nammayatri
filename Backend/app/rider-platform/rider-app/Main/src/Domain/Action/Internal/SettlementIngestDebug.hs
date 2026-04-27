module Domain.Action.Internal.SettlementIngestDebug (DebugSettlementIngestReq (..), debugSettlementIngest) where

import qualified Data.Map as Map
import Data.OpenApi (ToSchema)
import qualified Data.Text as T
import Domain.Types.Extra.SettlementOpenApi ()
import qualified EulerHS.Language as L
import Kernel.External.Encryption (EncFlow, encrypt)
import qualified Kernel.External.Settlement.Types as Settlement
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Utils.Servant.Client (HasRequestId)
import Lib.Finance.Settlement.Ingestion (IngestionResult, ingestPaymentSettlementReport)
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import Storage.Beam.Payment ()

-- | Local / dev only: plain SFTP password (and optional Juspay API key) are encrypted with app @EncTools@.
data DebugSettlementIngestReq = DebugSettlementIngestReq
  { merchantId :: Text,
    merchantOperatingCityId :: Text,
    settlementService :: Settlement.SettlementService,
    parserTypeMap :: Maybe Settlement.SettlementParserTypeMap,
    sftpHost :: Text,
    sftpPort :: Int,
    sftpUsername :: Text,
    sftpPassword :: Maybe Text,
    -- | Nothing or empty: @sftp ls -R@ from default landing dir; only @.csv@ / @.zip@ / @.csv.zip@ kept.
    sftpRemotePath :: Maybe Text,
    -- | When set with 'sftpPrivateKey64', decoded PEM is written here for @ssh -i@ (file is not deleted).
    sftpPrivateKeyPath :: Maybe Text,
    sftpPrivateKey64 :: Maybe Text,
    sftpCsvChunkRowLimit :: Maybe Int,
    juspayOrderStatusEnabled :: Maybe Bool,
    juspayBaseUrl :: Maybe Text,
    juspayApiKeyPlain :: Maybe Text
  }
  deriving (Generic, Show)

instance FromJSON DebugSettlementIngestReq

instance ToJSON DebugSettlementIngestReq

deriving anyclass instance ToSchema DebugSettlementIngestReq

debugSettlementIngest ::
  ( BeamFlow m r,
    EncFlow m r,
    MonadIO m,
    CoreMetrics m,
    L.MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  DebugSettlementIngestReq ->
  m IngestionResult
debugSettlementIngest DebugSettlementIngestReq {..} = do
  mbEncPassword <- case sftpPassword of
    Just pwd | not (T.null pwd) -> Just <$> encrypt pwd
    _ -> pure Nothing
  encJuspayKey <- traverse encrypt juspayApiKeyPlain
  let parserTyMap =
        fromMaybe (Settlement.SettlementParserTypeMap Map.empty) parserTypeMap
      remotePath =
        sftpRemotePath >>= \p -> if T.null p then Nothing else Just p
      sftpCfg =
        Settlement.SFTPConfig
          { host = sftpHost,
            port = sftpPort,
            username = sftpUsername,
            password = mbEncPassword,
            remotePath = remotePath,
            keyPath = sftpPrivateKeyPath,
            privateKey64 = sftpPrivateKey64,
            limit = Nothing,
            csvChunkRowLimit = sftpCsvChunkRowLimit
          }
      sourceConfig =
        -- mobility-core requires a second @Text@ on 'SFTPSourceConfig'; unused by finance-kernel SFTP fetch.
        Settlement.SFTPSourceConfig sftpCfg ""
      settlementCfg =
        Settlement.SettlementServiceConfig
          { settlementService = settlementService,
            sourceConfig = sourceConfig,
            parserTypeMap = parserTyMap,
            juspayOrderStatusEnabled = juspayOrderStatusEnabled,
            juspayBaseUrl = juspayBaseUrl,
            juspayApiKey = encJuspayKey
          }
  ingestPaymentSettlementReport settlementCfg merchantId merchantOperatingCityId
