{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Queries.OrphanInstances.PgSettlementConfig where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Finance.Domain.Types.PgSettlementConfig
import qualified Lib.Finance.Storage.Beam.PgSettlementConfig as Beam

instance FromTType' Beam.PgSettlementConfig Lib.Finance.Domain.Types.PgSettlementConfig.PgSettlementConfig where
  fromTType' (Beam.PgSettlementConfigT {..}) = do
    pure $
      Just
        Lib.Finance.Domain.Types.PgSettlementConfig.PgSettlementConfig
          { createdAt = createdAt,
            emailFolderName = emailFolderName,
            emailImapHost = emailImapHost,
            emailImapPort = emailImapPort,
            emailPassword = Encrypted <$> emailPassword,
            emailSubjectFilter = emailSubjectFilter,
            emailUsername = emailUsername,
            id = Kernel.Types.Id.Id id,
            ingestionEnabled = ingestionEnabled,
            lastIngestionAt = lastIngestionAt,
            lastIngestionStatus = lastIngestionStatus,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId,
            paymentGateway = paymentGateway,
            reportType = reportType,
            scheduledHourUtc = scheduledHourUtc,
            scheduledMinuteUtc = scheduledMinuteUtc,
            sftpFilePattern = sftpFilePattern,
            sftpHost = sftpHost,
            sftpPassword = Encrypted <$> sftpPassword,
            sftpPort = sftpPort,
            sftpRemotePath = sftpRemotePath,
            sftpUsername = sftpUsername,
            sourceType = sourceType,
            status = status,
            updatedAt = updatedAt
          }

instance ToTType' Beam.PgSettlementConfig Lib.Finance.Domain.Types.PgSettlementConfig.PgSettlementConfig where
  toTType' (Lib.Finance.Domain.Types.PgSettlementConfig.PgSettlementConfig {..}) = do
    Beam.PgSettlementConfigT
      { Beam.createdAt = createdAt,
        Beam.emailFolderName = emailFolderName,
        Beam.emailImapHost = emailImapHost,
        Beam.emailImapPort = emailImapPort,
        Beam.emailPassword = emailPassword <&> unEncrypted,
        Beam.emailSubjectFilter = emailSubjectFilter,
        Beam.emailUsername = emailUsername,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.ingestionEnabled = ingestionEnabled,
        Beam.lastIngestionAt = lastIngestionAt,
        Beam.lastIngestionStatus = lastIngestionStatus,
        Beam.merchantId = merchantId,
        Beam.merchantOperatingCityId = merchantOperatingCityId,
        Beam.paymentGateway = paymentGateway,
        Beam.reportType = reportType,
        Beam.scheduledHourUtc = scheduledHourUtc,
        Beam.scheduledMinuteUtc = scheduledMinuteUtc,
        Beam.sftpFilePattern = sftpFilePattern,
        Beam.sftpHost = sftpHost,
        Beam.sftpPassword = sftpPassword <&> unEncrypted,
        Beam.sftpPort = sftpPort,
        Beam.sftpRemotePath = sftpRemotePath,
        Beam.sftpUsername = sftpUsername,
        Beam.sourceType = sourceType,
        Beam.status = status,
        Beam.updatedAt = updatedAt
      }
