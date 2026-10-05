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
    attemptCount :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
    batchId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    deliveredAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    driverId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    emailAddress :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    failureReason :: (B.C f (Kernel.Prelude.Maybe Domain.Types.TDSDistributionRecord.TDSFailureReason)),
    fileName :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    financialYear :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    id :: (B.C f Kernel.Prelude.Text),
    lastAttemptAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    latestEmailDeliveryId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    quarter :: (B.C f Kernel.Prelude.Text),
    retryCount :: (B.C f Kernel.Prelude.Int),
    status :: (B.C f Domain.Types.TDSDistributionRecord.TDSDistributionStatus),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table TDSDistributionRecordT where
  data PrimaryKey TDSDistributionRecordT f = TDSDistributionRecordId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = TDSDistributionRecordId . id

type TDSDistributionRecord = TDSDistributionRecordT Identity

$(enableKVPG (''TDSDistributionRecordT) [('id)] [[('batchId)], [('driverId)]])

$(mkTableInstances (''TDSDistributionRecordT) "tds_distribution_record")
