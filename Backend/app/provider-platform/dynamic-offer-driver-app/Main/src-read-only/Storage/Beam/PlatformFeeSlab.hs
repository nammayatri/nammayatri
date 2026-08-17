{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.PlatformFeeSlab where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data PlatformFeeSlabT f = PlatformFeeSlabT
  { cgstPercentage :: B.C f Kernel.Types.Common.HighPrecMoney,
    maxDistanceMeters :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    minDistanceMeters :: B.C f Kernel.Prelude.Int,
    platformFee :: B.C f Kernel.Types.Common.HighPrecMoney,
    sgstPercentage :: B.C f Kernel.Types.Common.HighPrecMoney,
    slabSetId :: B.C f Kernel.Prelude.Text,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table PlatformFeeSlabT where
  data PrimaryKey PlatformFeeSlabT f
    = PlatformFeeSlabId (B.C f Kernel.Prelude.Int) (B.C f Kernel.Prelude.Text)
    deriving (Generic, B.Beamable)
  primaryKey = PlatformFeeSlabId <$> minDistanceMeters <*> slabSetId

type PlatformFeeSlab = PlatformFeeSlabT Identity

$(enableKVPG ''PlatformFeeSlabT ['minDistanceMeters, 'slabSetId] [])

$(mkTableInstances ''PlatformFeeSlabT "platform_fee_slab")
