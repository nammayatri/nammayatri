{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.DeviceVehicleMapping where

import qualified Data.Text
import qualified Data.Time
import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data DeviceVehicleMappingT f = DeviceVehicleMappingT
  { createdAt :: (B.C f Data.Time.UTCTime),
    deviceId :: (B.C f Data.Text.Text),
    gtfsId :: (B.C f Data.Text.Text),
    updatedAt :: (B.C f Data.Time.UTCTime),
    vehicleNo :: (B.C f Data.Text.Text),
    merchantId :: (B.C f (Kernel.Prelude.Maybe (Data.Text.Text))),
    merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Data.Text.Text)))
  }
  deriving (Generic, B.Beamable)

instance B.Table DeviceVehicleMappingT where
  data PrimaryKey DeviceVehicleMappingT f = DeviceVehicleMappingId (B.C f Data.Text.Text) deriving (Generic, B.Beamable)
  primaryKey = DeviceVehicleMappingId . deviceId

type DeviceVehicleMapping = DeviceVehicleMappingT Identity

$(enableKVPG (''DeviceVehicleMappingT) [('deviceId)] [])

$(mkTableInstances (''DeviceVehicleMappingT) "device_vehicle_mapping")
