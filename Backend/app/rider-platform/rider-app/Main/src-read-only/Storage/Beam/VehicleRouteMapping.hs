{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.VehicleRouteMapping where

import qualified Data.Time
import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data VehicleRouteMappingT f = VehicleRouteMappingT
  { createdAt :: B.C f Data.Time.UTCTime,
    routeId :: B.C f Kernel.Prelude.Text,
    service :: B.C f Kernel.Prelude.Text,
    shift :: B.C f Kernel.Prelude.Text,
    typeOfService :: B.C f Kernel.Prelude.Text,
    updatedAt :: B.C f Data.Time.UTCTime,
    vehicleNo :: B.C f Kernel.Prelude.Text
  }
  deriving (Generic, B.Beamable)

instance B.Table VehicleRouteMappingT where
  data PrimaryKey VehicleRouteMappingT f = VehicleRouteMappingId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = VehicleRouteMappingId . vehicleNo

type VehicleRouteMapping = VehicleRouteMappingT Identity

$(enableKVPG ''VehicleRouteMappingT ['vehicleNo] [])

$(mkTableInstances ''VehicleRouteMappingT "vehicle_route_mapping")
