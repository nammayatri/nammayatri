{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.TowingVehicle where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data TowingVehicleT f = TowingVehicleT
  { agencyEntityId :: (B.C f Kernel.Prelude.Text),
    id :: (B.C f Kernel.Prelude.Text),
    isActive :: (B.C f Kernel.Prelude.Bool),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    vehicleNumber :: (B.C f Kernel.Prelude.Text),
    vehicleType :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table TowingVehicleT where
  data PrimaryKey TowingVehicleT f = TowingVehicleId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = TowingVehicleId . id

type TowingVehicle = TowingVehicleT Identity

$(enableKVPG (''TowingVehicleT) [('id)] [[('agencyEntityId)]])

$(mkTableInstances (''TowingVehicleT) "towing_vehicle")
