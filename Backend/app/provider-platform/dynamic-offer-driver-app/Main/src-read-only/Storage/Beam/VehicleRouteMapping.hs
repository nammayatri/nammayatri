{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.VehicleRouteMapping where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Data.Text
import qualified Kernel.External.Encryption
import qualified Database.Beam as B



data VehicleRouteMappingT f
    = VehicleRouteMappingT {blocked :: (B.C f Kernel.Prelude.Bool),
                            fleetOwnerId :: (B.C f Data.Text.Text),
                            merchantId :: (B.C f Data.Text.Text),
                            merchantOperatingCityId :: (B.C f Data.Text.Text),
                            routeCode :: (B.C f Data.Text.Text),
                            vehicleNumberEncrypted :: (B.C f Data.Text.Text),
                            vehicleNumberHash :: (B.C f Kernel.External.Encryption.DbHash),
                            createdAt :: (B.C f Kernel.Prelude.UTCTime),
                            updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table VehicleRouteMappingT
    where data PrimaryKey VehicleRouteMappingT f = VehicleRouteMappingId (B.C f Data.Text.Text) (B.C f Kernel.External.Encryption.DbHash) deriving (Generic, B.Beamable)
          primaryKey = VehicleRouteMappingId <$> routeCode <*> vehicleNumberHash
type VehicleRouteMapping = VehicleRouteMappingT Identity

$(enableKVPG (''VehicleRouteMappingT) [('routeCode), ('vehicleNumberHash)] [])

$(mkTableInstances (''VehicleRouteMappingT) "vehicle_route_mapping")

