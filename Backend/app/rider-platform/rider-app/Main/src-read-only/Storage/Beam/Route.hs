{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.Route where

import qualified BecknV2.FRFS.Enums
import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.TimeBound
import Tools.Beam.UtilsTH

data RouteT f = RouteT
  { code :: B.C f Kernel.Prelude.Text,
    color :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    endLat :: B.C f Kernel.Prelude.Double,
    endLon :: B.C f Kernel.Prelude.Double,
    id :: B.C f Kernel.Prelude.Text,
    integratedBppConfigId :: B.C f Kernel.Prelude.Text,
    longName :: B.C f Kernel.Prelude.Text,
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    polyline :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    shortName :: B.C f Kernel.Prelude.Text,
    startLat :: B.C f Kernel.Prelude.Double,
    startLon :: B.C f Kernel.Prelude.Double,
    timeBounds :: B.C f Kernel.Types.TimeBound.TimeBound,
    vehicleType :: B.C f BecknV2.FRFS.Enums.VehicleCategory,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table RouteT where
  data PrimaryKey RouteT f = RouteId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = RouteId . id

type Route = RouteT Identity

$(enableKVPG ''RouteT ['id] [['code]])

$(mkTableInstances ''RouteT "route")
