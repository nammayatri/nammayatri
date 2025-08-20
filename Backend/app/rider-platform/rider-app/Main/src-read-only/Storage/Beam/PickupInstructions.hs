{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.PickupInstructions where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data PickupInstructionsT f = PickupInstructionsT
  { createdAt :: B.C f Kernel.Prelude.UTCTime,
    geohash :: B.C f Kernel.Prelude.Text,
    id :: B.C f Kernel.Prelude.Text,
    instruction :: B.C f Kernel.Prelude.Text,
    mediaFileId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    personId :: B.C f Kernel.Prelude.Text,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table PickupInstructionsT where
  data PrimaryKey PickupInstructionsT f = PickupInstructionsId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = PickupInstructionsId . id

type PickupInstructions = PickupInstructionsT Identity

$(enableKVPG ''PickupInstructionsT ['id] [['personId]])

$(mkTableInstances ''PickupInstructionsT "pickup_instructions")
