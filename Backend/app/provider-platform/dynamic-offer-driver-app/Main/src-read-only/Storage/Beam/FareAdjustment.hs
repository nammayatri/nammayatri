{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FareAdjustment where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.FareAdjustment
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data FareAdjustmentT f = FareAdjustmentT
  { areas :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    baseFareScalePct :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double)),
    congestionScalePct :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double)),
    createdBy :: (B.C f Kernel.Prelude.Text),
    id :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    mode :: (B.C f Domain.Types.FareAdjustment.FareAdjustmentMode),
    perKmRateScalePct :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double)),
    perMinRateScalePct :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double)),
    reason :: (B.C f Kernel.Prelude.Text),
    rolloutPercentage :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
    status :: (B.C f Domain.Types.FareAdjustment.FareAdjustmentStatus),
    validFrom :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    validTill :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    vehicleServiceTiers :: (B.C f Kernel.Prelude.Text),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table FareAdjustmentT where
  data PrimaryKey FareAdjustmentT f = FareAdjustmentId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = FareAdjustmentId . id

type FareAdjustment = FareAdjustmentT Identity

$(enableKVPG (''FareAdjustmentT) [('id)] [])

$(mkTableInstances (''FareAdjustmentT) "fare_adjustment")
