{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.VehicleSeatLayoutMapping where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Database.Beam as B



data VehicleSeatLayoutMappingT f
    = VehicleSeatLayoutMappingT {gtfsId :: (B.C f Kernel.Prelude.Text),
                                 id :: (B.C f Kernel.Prelude.Text),
                                 merchantId :: (B.C f Kernel.Prelude.Text),
                                 merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
                                 seatLayoutId :: (B.C f Kernel.Prelude.Text),
                                 vehicleNo :: (B.C f Kernel.Prelude.Text),
                                 createdAt :: (B.C f Kernel.Prelude.UTCTime),
                                 updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table VehicleSeatLayoutMappingT
    where data PrimaryKey VehicleSeatLayoutMappingT f = VehicleSeatLayoutMappingId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = VehicleSeatLayoutMappingId . id
type VehicleSeatLayoutMapping = VehicleSeatLayoutMappingT Identity

$(enableKVPG (''VehicleSeatLayoutMappingT) [('id)] [])

$(mkTableInstances (''VehicleSeatLayoutMappingT) "vehicle_seat_layout_mapping")

