{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.DriverHomeLocation where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data DriverHomeLocationT f = DriverHomeLocationT
  { address :: B.C f Kernel.Prelude.Text,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    driverId :: B.C f Kernel.Prelude.Text,
    id :: B.C f Kernel.Prelude.Text,
    lat :: B.C f Kernel.Prelude.Double,
    lon :: B.C f Kernel.Prelude.Double,
    tag :: B.C f Kernel.Prelude.Text,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table DriverHomeLocationT where
  data PrimaryKey DriverHomeLocationT f = DriverHomeLocationId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = DriverHomeLocationId . id

type DriverHomeLocation = DriverHomeLocationT Identity

$(enableKVPG ''DriverHomeLocationT ['id] [['driverId]])

$(mkTableInstancesWithTModifier ''DriverHomeLocationT "driver_home_location" [("address", "home_address")])
