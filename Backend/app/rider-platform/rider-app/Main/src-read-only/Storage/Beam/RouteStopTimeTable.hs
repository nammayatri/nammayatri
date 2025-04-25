{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.RouteStopTimeTable where

import qualified BecknV2.FRFS.Enums
import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data RouteStopTimeTableT f = RouteStopTimeTableT
  { integratedBppConfigId :: B.C f Kernel.Prelude.Text,
    routeCode :: B.C f Kernel.Prelude.Text,
    serviceTierType :: B.C f BecknV2.FRFS.Enums.ServiceTierType,
    stopCode :: B.C f Kernel.Prelude.Text,
    timeOfArrival :: B.C f Kernel.Prelude.TimeOfDay,
    timeOfDeparture :: B.C f Kernel.Prelude.TimeOfDay,
    tripId :: B.C f Kernel.Prelude.Text,
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table RouteStopTimeTableT where
  data PrimaryKey RouteStopTimeTableT f
    = RouteStopTimeTableId (B.C f Kernel.Prelude.Text) (B.C f BecknV2.FRFS.Enums.ServiceTierType) (B.C f Kernel.Prelude.Text) (B.C f Kernel.Prelude.TimeOfDay) (B.C f Kernel.Prelude.Text)
    deriving (Generic, B.Beamable)
  primaryKey = RouteStopTimeTableId <$> integratedBppConfigId <*> serviceTierType <*> stopCode <*> timeOfArrival <*> tripId

type RouteStopTimeTable = RouteStopTimeTableT Identity

$(enableKVPG ''RouteStopTimeTableT ['integratedBppConfigId, 'serviceTierType, 'stopCode, 'timeOfArrival, 'tripId] [])

$(mkTableInstances ''RouteStopTimeTableT "route_stop_time_table")
