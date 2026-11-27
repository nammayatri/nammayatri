{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.SharedCabSlotCapacity where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data SharedCabSlotCapacityT f = SharedCabSlotCapacityT
  { capacity :: (B.C f Kernel.Prelude.Int),
    id :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    routeId :: (B.C f Kernel.Prelude.Text),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table SharedCabSlotCapacityT where
  data PrimaryKey SharedCabSlotCapacityT f = SharedCabSlotCapacityId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = SharedCabSlotCapacityId . id

type SharedCabSlotCapacity = SharedCabSlotCapacityT Identity

$(enableKVPG (''SharedCabSlotCapacityT) [('id)] [[('routeId)]])

$(mkTableInstances (''SharedCabSlotCapacityT) "shared_cab_slot_capacity")
