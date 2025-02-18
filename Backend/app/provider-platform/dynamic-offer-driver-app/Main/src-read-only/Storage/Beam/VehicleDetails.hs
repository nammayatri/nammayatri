{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.VehicleDetails where

import qualified Data.Text
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.VehicleVariant
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data VehicleDetailsT f = VehicleDetailsT
  { acAvailable :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    capacity :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    id :: B.C f Data.Text.Text,
    make :: B.C f Data.Text.Text,
    model :: B.C f Data.Text.Text,
    vehicleVariant :: B.C f Domain.Types.VehicleVariant.VehicleVariant,
    year :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)
  }
  deriving (Generic, B.Beamable)

instance B.Table VehicleDetailsT where
  data PrimaryKey VehicleDetailsT f = VehicleDetailsId (B.C f Data.Text.Text) deriving (Generic, B.Beamable)
  primaryKey = VehicleDetailsId . id

type VehicleDetails = VehicleDetailsT Identity

$(enableKVPG ''VehicleDetailsT ['id] [])

$(mkTableInstances ''VehicleDetailsT "vehicle_details")
