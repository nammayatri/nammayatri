{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Domain.Types.PgSettlementConfig where

import Data.Aeson
import qualified Kernel.Beam.Lib.UtilsTH
import qualified Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Kernel.Utils.TH

data PgSettlementConfig = PgSettlementConfig
  { createdAt :: Kernel.Prelude.UTCTime,
    emailFolderName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    emailImapHost :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    emailImapPort :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    emailPassword :: Kernel.Prelude.Maybe (Kernel.External.Encryption.EncryptedField 'Kernel.External.Encryption.AsEncrypted Kernel.Prelude.Text),
    emailSubjectFilter :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    emailUsername :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Lib.Finance.Domain.Types.PgSettlementConfig.PgSettlementConfig,
    ingestionEnabled :: Kernel.Prelude.Bool,
    lastIngestionAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    lastIngestionStatus :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    merchantId :: Kernel.Prelude.Text,
    merchantOperatingCityId :: Kernel.Prelude.Text,
    paymentGateway :: Lib.Finance.Domain.Types.PgSettlementConfig.PaymentGateway,
    reportType :: Lib.Finance.Domain.Types.PgSettlementConfig.ReportType,
    scheduledHourUtc :: Kernel.Prelude.Int,
    scheduledMinuteUtc :: Kernel.Prelude.Int,
    sftpFilePattern :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    sftpHost :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    sftpPassword :: Kernel.Prelude.Maybe (Kernel.External.Encryption.EncryptedField 'Kernel.External.Encryption.AsEncrypted Kernel.Prelude.Text),
    sftpPort :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    sftpRemotePath :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    sftpUsername :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    sourceType :: Lib.Finance.Domain.Types.PgSettlementConfig.SourceType,
    status :: Lib.Finance.Domain.Types.PgSettlementConfig.ConfigStatus,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic)

data ConfigStatus = ACTIVE | INACTIVE deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data PaymentGateway = HYPER_PG | BILL_DESK deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data ReportType = PAYMENT | PAYOUT deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data SourceType = EMAIL | SFTP deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList (''ConfigStatus))

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList (''PaymentGateway))

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList (''ReportType))

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList (''SourceType))

$(Kernel.Utils.TH.mkHttpInstancesForEnum (''PaymentGateway))

$(Kernel.Utils.TH.mkHttpInstancesForEnum (''SourceType))

$(Kernel.Utils.TH.mkHttpInstancesForEnum (''ReportType))

$(Kernel.Utils.TH.mkHttpInstancesForEnum (''ConfigStatus))
