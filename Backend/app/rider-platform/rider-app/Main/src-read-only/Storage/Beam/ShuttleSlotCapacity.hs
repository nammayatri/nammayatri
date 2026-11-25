{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.ShuttleSlotCapacity where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data ShuttleSlotCapacityT f = ShuttleSlotCapacityT
  { capacity :: (B.C f Kernel.Prelude.Int),
    id :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    tripId :: (B.C f Kernel.Prelude.Text),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table ShuttleSlotCapacityT where
  data PrimaryKey ShuttleSlotCapacityT f = ShuttleSlotCapacityId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = ShuttleSlotCapacityId . id

type ShuttleSlotCapacity = ShuttleSlotCapacityT Identity

$(enableKVPG (''ShuttleSlotCapacityT) [('id)] [[('tripId)]])

$(mkTableInstances (''ShuttleSlotCapacityT) "shuttle_slot_capacity")
