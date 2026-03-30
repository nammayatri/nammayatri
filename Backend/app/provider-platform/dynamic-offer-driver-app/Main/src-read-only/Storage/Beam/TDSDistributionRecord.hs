{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.TDSDistributionRecord where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.TDSDistributionRecord
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data TDSDistributionRecordT f = TDSDistributionRecordT
  { assessmentYear :: (B.C f Kernel.Prelude.Text),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    driverId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    emailAddress :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    fileName :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    id :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    quarter :: (B.C f Kernel.Prelude.Text),
    retryCount :: (B.C f Kernel.Prelude.Int),
    s3FilePath :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    status :: (B.C f Domain.Types.TDSDistributionRecord.TDSDistributionStatus),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table TDSDistributionRecordT where
  data PrimaryKey TDSDistributionRecordT f = TDSDistributionRecordId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = TDSDistributionRecordId . id

type TDSDistributionRecord = TDSDistributionRecordT Identity

$(enableKVPG (''TDSDistributionRecordT) [('id)] [[('driverId)]])

$(mkTableInstances (''TDSDistributionRecordT) "tds_distribution_record")
