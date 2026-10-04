{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.TDSDistributionBatch where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.TDSDistributionBatch
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data TDSDistributionBatchT f = TDSDistributionBatchT
  { completedAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    confirmedAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    confirmedById :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    confirmedByName :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    financialYear :: (B.C f Kernel.Prelude.Text),
    folderName :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    id :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    quarter :: (B.C f Kernel.Prelude.Text),
    status :: (B.C f Domain.Types.TDSDistributionBatch.TDSDistributionBatchStatus),
    totalFiles :: (B.C f Kernel.Prelude.Int),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime),
    uploadedById :: (B.C f Kernel.Prelude.Text),
    uploadedByName :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    validatedAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime))
  }
  deriving (Generic, B.Beamable)

instance B.Table TDSDistributionBatchT where
  data PrimaryKey TDSDistributionBatchT f = TDSDistributionBatchId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = TDSDistributionBatchId . id

type TDSDistributionBatch = TDSDistributionBatchT Identity

$(enableKVPG (''TDSDistributionBatchT) [('id)] [])

$(mkTableInstances (''TDSDistributionBatchT) "tds_distribution_batch")
