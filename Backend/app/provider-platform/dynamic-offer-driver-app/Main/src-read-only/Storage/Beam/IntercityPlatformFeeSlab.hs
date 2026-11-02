{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.IntercityPlatformFeeSlab where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data IntercityPlatformFeeSlabT f = IntercityPlatformFeeSlabT
  { cgstPercentage :: B.C f Kernel.Types.Common.HighPrecMoney,
    maxDistanceMeters :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    minDistanceMeters :: B.C f Kernel.Prelude.Int,
    platformFee :: B.C f Kernel.Types.Common.HighPrecMoney,
    sgstPercentage :: B.C f Kernel.Types.Common.HighPrecMoney,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table IntercityPlatformFeeSlabT where
  data PrimaryKey IntercityPlatformFeeSlabT f
    = IntercityPlatformFeeSlabId (B.C f Kernel.Prelude.Text) (B.C f Kernel.Prelude.Int)
    deriving (Generic, B.Beamable)
  primaryKey = IntercityPlatformFeeSlabId <$> merchantOperatingCityId <*> minDistanceMeters

type IntercityPlatformFeeSlab = IntercityPlatformFeeSlabT Identity

$(enableKVPG ''IntercityPlatformFeeSlabT ['merchantOperatingCityId, 'minDistanceMeters] [])

$(mkTableInstances ''IntercityPlatformFeeSlabT "intercity_platform_fee_slab")
