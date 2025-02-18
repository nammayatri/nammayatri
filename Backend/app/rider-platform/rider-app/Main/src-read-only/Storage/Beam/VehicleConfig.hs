{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.VehicleConfig where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data VehicleConfigT f = VehicleConfigT
  { becknConfigId :: B.C f Kernel.Prelude.Text,
    blackListedSubscribers :: B.C f [Kernel.Prelude.Text],
    buyerFinderFee :: B.C f Kernel.Prelude.Text,
    category :: B.C f Kernel.Prelude.Text,
    id :: B.C f Kernel.Prelude.Text,
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table VehicleConfigT where
  data PrimaryKey VehicleConfigT f = VehicleConfigId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = VehicleConfigId . id

type VehicleConfig = VehicleConfigT Identity

$(enableKVPG ''VehicleConfigT ['id] [])

$(mkTableInstances ''VehicleConfigT "beckn_vehicle_config")
