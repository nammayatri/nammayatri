{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.RoutePolylines where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified BecknV2.FRFS.Enums
import qualified Database.Beam as B



data RoutePolylinesT f
    = RoutePolylinesT {createdAt :: (B.C f Kernel.Prelude.UTCTime),
                       id :: (B.C f Kernel.Prelude.Text),
                       merchantId :: (B.C f Kernel.Prelude.Text),
                       merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
                       polyline :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                       routeId :: (B.C f Kernel.Prelude.Text),
                       updatedAt :: (B.C f Kernel.Prelude.UTCTime),
                       vehicleType :: (B.C f BecknV2.FRFS.Enums.VehicleCategory)}
    deriving (Generic, B.Beamable)
instance B.Table RoutePolylinesT
    where data PrimaryKey RoutePolylinesT f = RoutePolylinesId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = RoutePolylinesId . id
type RoutePolylines = RoutePolylinesT Identity

$(enableKVPG (''RoutePolylinesT) [('id)] [])

$(mkTableInstances (''RoutePolylinesT) "route_polylines")

