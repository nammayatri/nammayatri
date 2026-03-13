{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Queries.PgSettlementConfig (module Lib.Finance.Storage.Queries.PgSettlementConfig, module ReExport) where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Finance.Domain.Types.PgSettlementConfig
import qualified Lib.Finance.Storage.Beam.BeamFlow
import qualified Lib.Finance.Storage.Beam.PgSettlementConfig as Beam
import Lib.Finance.Storage.Queries.PgSettlementConfigExtra as ReExport
import qualified Sequelize as Se

create :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Finance.Domain.Types.PgSettlementConfig.PgSettlementConfig -> m ())
create = createWithKV

createMany :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => ([Lib.Finance.Domain.Types.PgSettlementConfig.PgSettlementConfig] -> m ())
createMany = traverse_ create

findAllActiveByMerchantId ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Prelude.Text -> Kernel.Prelude.Bool -> Lib.Finance.Domain.Types.PgSettlementConfig.ConfigStatus -> m ([Lib.Finance.Domain.Types.PgSettlementConfig.PgSettlementConfig]))
findAllActiveByMerchantId merchantId ingestionEnabled status = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantId $ Se.Eq merchantId,
          Se.Is Beam.ingestionEnabled $ Se.Eq ingestionEnabled,
          Se.Is Beam.status $ Se.Eq status
        ]
    ]

findAllActiveByMerchantIdAndReportType ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Prelude.Text -> Kernel.Prelude.Bool -> Lib.Finance.Domain.Types.PgSettlementConfig.ConfigStatus -> Lib.Finance.Domain.Types.PgSettlementConfig.ReportType -> m ([Lib.Finance.Domain.Types.PgSettlementConfig.PgSettlementConfig]))
findAllActiveByMerchantIdAndReportType merchantId ingestionEnabled status reportType = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantId $ Se.Eq merchantId,
          Se.Is Beam.ingestionEnabled $ Se.Eq ingestionEnabled,
          Se.Is Beam.status $ Se.Eq status,
          Se.Is Beam.reportType $ Se.Eq reportType
        ]
    ]

findById ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Finance.Domain.Types.PgSettlementConfig.PgSettlementConfig -> m (Maybe Lib.Finance.Domain.Types.PgSettlementConfig.PgSettlementConfig))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByMerchantIdAndPaymentGatewayAndReportType ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Prelude.Text -> Kernel.Prelude.Text -> Lib.Finance.Domain.Types.PgSettlementConfig.PaymentGateway -> Lib.Finance.Domain.Types.PgSettlementConfig.ReportType -> m (Maybe Lib.Finance.Domain.Types.PgSettlementConfig.PgSettlementConfig))
findByMerchantIdAndPaymentGatewayAndReportType merchantId merchantOperatingCityId paymentGateway reportType = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.merchantId $ Se.Eq merchantId,
          Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOperatingCityId,
          Se.Is Beam.paymentGateway $ Se.Eq paymentGateway,
          Se.Is Beam.reportType $ Se.Eq reportType
        ]
    ]

updateLastIngestion ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id Lib.Finance.Domain.Types.PgSettlementConfig.PgSettlementConfig -> m ())
updateLastIngestion lastIngestionAt lastIngestionStatus id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.lastIngestionAt lastIngestionAt, Se.Set Beam.lastIngestionStatus lastIngestionStatus, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Finance.Domain.Types.PgSettlementConfig.PgSettlementConfig -> m (Maybe Lib.Finance.Domain.Types.PgSettlementConfig.PgSettlementConfig))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Finance.Domain.Types.PgSettlementConfig.PgSettlementConfig -> m ())
updateByPrimaryKey (Lib.Finance.Domain.Types.PgSettlementConfig.PgSettlementConfig {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.emailFolderName emailFolderName,
      Se.Set Beam.emailImapHost emailImapHost,
      Se.Set Beam.emailImapPort emailImapPort,
      Se.Set Beam.emailPassword (emailPassword <&> unEncrypted),
      Se.Set Beam.emailSubjectFilter emailSubjectFilter,
      Se.Set Beam.emailUsername emailUsername,
      Se.Set Beam.ingestionEnabled ingestionEnabled,
      Se.Set Beam.lastIngestionAt lastIngestionAt,
      Se.Set Beam.lastIngestionStatus lastIngestionStatus,
      Se.Set Beam.merchantId merchantId,
      Se.Set Beam.merchantOperatingCityId merchantOperatingCityId,
      Se.Set Beam.paymentGateway paymentGateway,
      Se.Set Beam.reportType reportType,
      Se.Set Beam.scheduledHourUtc scheduledHourUtc,
      Se.Set Beam.scheduledMinuteUtc scheduledMinuteUtc,
      Se.Set Beam.sftpFilePattern sftpFilePattern,
      Se.Set Beam.sftpHost sftpHost,
      Se.Set Beam.sftpPassword (sftpPassword <&> unEncrypted),
      Se.Set Beam.sftpPort sftpPort,
      Se.Set Beam.sftpRemotePath sftpRemotePath,
      Se.Set Beam.sftpUsername sftpUsername,
      Se.Set Beam.sourceType sourceType,
      Se.Set Beam.status status,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
