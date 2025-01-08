{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.VehicleRouteMapping where

import qualified Data.Text
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.Common
import Kernel.External.Encryption
import qualified Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data VehicleRouteMappingT f = VehicleRouteMappingT
  { blocked :: (B.C f Kernel.Prelude.Bool),
    fleetOwnerId :: (B.C f Data.Text.Text),
    routeCode :: (B.C f Data.Text.Text),
    vehicleClass :: (B.C f Data.Text.Text),
    vehicleColor :: (B.C f Data.Text.Text),
    vehicleModel :: (B.C f Data.Text.Text),
    vehicleNumberEncrypted :: (B.C f Data.Text.Text),
    vehicleNumberHash :: (B.C f Kernel.External.Encryption.DbHash),
    vehicleServiceTierType :: (B.C f Domain.Types.Common.ServiceTierType),
    merchantId :: (B.C f (Kernel.Prelude.Maybe (Data.Text.Text))),
    merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Data.Text.Text))),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table VehicleRouteMappingT where
  data PrimaryKey VehicleRouteMappingT f = VehicleRouteMappingId (B.C f Data.Text.Text) deriving (Generic, B.Beamable)
  primaryKey = VehicleRouteMappingId . routeCode

type VehicleRouteMapping = VehicleRouteMappingT Identity

$(enableKVPG (''VehicleRouteMappingT) [('routeCode)] [])

$(mkTableInstances (''VehicleRouteMappingT) "vehicle_route_mapping")
