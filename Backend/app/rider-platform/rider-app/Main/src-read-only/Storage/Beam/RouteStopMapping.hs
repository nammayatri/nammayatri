{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.RouteStopMapping where

import qualified BecknV2.FRFS.Enums
import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Time
import qualified Kernel.Types.TimeBound
import Tools.Beam.UtilsTH

data RouteStopMappingT f = RouteStopMappingT
  { estimatedTravelTimeFromPreviousStop :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Time.Seconds),
    integratedBppConfigId :: B.C f Kernel.Prelude.Text,
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    providerCode :: B.C f Kernel.Prelude.Text,
    routeCode :: B.C f Kernel.Prelude.Text,
    sequenceNum :: B.C f Kernel.Prelude.Int,
    stopCode :: B.C f Kernel.Prelude.Text,
    stopName :: B.C f Kernel.Prelude.Text,
    stopLat :: B.C f Kernel.Prelude.Double,
    stopLon :: B.C f Kernel.Prelude.Double,
    timeBounds :: B.C f Kernel.Types.TimeBound.TimeBound,
    vehicleType :: B.C f BecknV2.FRFS.Enums.VehicleCategory,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table RouteStopMappingT where
  data PrimaryKey RouteStopMappingT f = RouteStopMappingId (B.C f Kernel.Prelude.Text) (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = RouteStopMappingId <$> routeCode <*> stopCode

type RouteStopMapping = RouteStopMappingT Identity

$(enableKVPG ''RouteStopMappingT ['routeCode, 'stopCode] [])

$(mkTableInstances ''RouteStopMappingT "route_stop_mapping")
