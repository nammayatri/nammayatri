{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.RouteTripMapping where

import qualified BecknV2.FRFS.Enums
import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data RouteTripMappingT f = RouteTripMappingT
  { createdAt :: B.C f Kernel.Prelude.UTCTime,
    integratedBppConfigId :: B.C f Kernel.Prelude.Text,
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    routeCode :: B.C f Kernel.Prelude.Text,
    tripCode :: B.C f Kernel.Prelude.Text,
    tripEndTime :: B.C f Kernel.Prelude.Text,
    tripStartTime :: B.C f Kernel.Prelude.Text,
    updatedAt :: B.C f Kernel.Prelude.UTCTime,
    vehicleType :: B.C f BecknV2.FRFS.Enums.VehicleCategory
  }
  deriving (Generic, B.Beamable)

instance B.Table RouteTripMappingT where
  data PrimaryKey RouteTripMappingT f = RouteTripMappingId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = RouteTripMappingId . tripCode

type RouteTripMapping = RouteTripMappingT Identity

$(enableKVPG ''RouteTripMappingT ['tripCode] [])

$(mkTableInstances ''RouteTripMappingT "route_trip_mapping")
