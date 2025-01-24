{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.Rollout where

import qualified BecknV2.FRFS.Enums
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.Extra.Rollout
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data RolloutT f = RolloutT
  { id :: (B.C f Kernel.Prelude.Text),
    inputDataType :: (B.C f Domain.Types.Extra.Rollout.RawDataType),
    percentage :: (B.C f Kernel.Prelude.Int),
    vehicleType :: (B.C f BecknV2.FRFS.Enums.VehicleCategory),
    versionTag :: (B.C f Kernel.Prelude.Int),
    merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table RolloutT where
  data PrimaryKey RolloutT f = RolloutId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = RolloutId . id

type Rollout = RolloutT Identity

$(enableKVPG (''RolloutT) [('id)] [])

$(mkTableInstances (''RolloutT) "rollout")
