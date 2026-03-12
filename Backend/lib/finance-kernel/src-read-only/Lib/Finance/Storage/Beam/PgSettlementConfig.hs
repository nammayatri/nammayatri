{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Beam.PgSettlementConfig where

import qualified Database.Beam as B
import Kernel.Beam.Lib.UtilsTH
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Lib.Finance.Domain.Types.PgSettlementConfig

data PgSettlementConfigT f = PgSettlementConfigT
  { createdAt :: (B.C f Kernel.Prelude.UTCTime),
    emailFolderName :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    emailImapHost :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    emailImapPort :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
    emailPassword :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    emailSubjectFilter :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    emailUsername :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    id :: (B.C f Kernel.Prelude.Text),
    ingestionEnabled :: (B.C f Kernel.Prelude.Bool),
    lastIngestionAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    lastIngestionStatus :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    paymentGateway :: (B.C f Lib.Finance.Domain.Types.PgSettlementConfig.PaymentGateway),
    reportType :: (B.C f Lib.Finance.Domain.Types.PgSettlementConfig.ReportType),
    scheduledHourUtc :: (B.C f Kernel.Prelude.Int),
    scheduledMinuteUtc :: (B.C f Kernel.Prelude.Int),
    sftpFilePattern :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    sftpHost :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    sftpPassword :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    sftpPort :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
    sftpRemotePath :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    sftpUsername :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    sourceType :: (B.C f Lib.Finance.Domain.Types.PgSettlementConfig.SourceType),
    status :: (B.C f Lib.Finance.Domain.Types.PgSettlementConfig.ConfigStatus),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table PgSettlementConfigT where
  data PrimaryKey PgSettlementConfigT f = PgSettlementConfigId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = PgSettlementConfigId . id

type PgSettlementConfig = PgSettlementConfigT Identity

$(enableKVPG (''PgSettlementConfigT) [('id)] [])

$(mkTableInstancesGenericSchema (''PgSettlementConfigT) "pg_settlement_config")
