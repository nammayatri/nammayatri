{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.DeviceVehicleMapping where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Data.Time
import qualified Data.Text
import qualified Kernel.Prelude
import qualified Database.Beam as B



data DeviceVehicleMappingT f
    = DeviceVehicleMappingT {createdAt :: (B.C f Data.Time.UTCTime),
                             deviceId :: (B.C f Data.Text.Text),
                             gtfsId :: (B.C f Data.Text.Text),
                             updatedAt :: (B.C f Data.Time.UTCTime),
                             vehicleNo :: (B.C f Data.Text.Text),
                             merchantId :: (B.C f (Kernel.Prelude.Maybe (Data.Text.Text))),
                             merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Data.Text.Text)))}
    deriving (Generic, B.Beamable)
instance B.Table DeviceVehicleMappingT
    where data PrimaryKey DeviceVehicleMappingT f = DeviceVehicleMappingId (B.C f Data.Text.Text) (B.C f Data.Text.Text) deriving (Generic, B.Beamable)
          primaryKey = DeviceVehicleMappingId <$> deviceId <*> gtfsId
type DeviceVehicleMapping = DeviceVehicleMappingT Identity

$(enableKVPG (''DeviceVehicleMappingT) [('deviceId), ('gtfsId)] [])

$(mkTableInstances (''DeviceVehicleMappingT) "device_vehicle_mapping")

