{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.Route where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Kernel.Types.TimeBound
import qualified Domain.Types.VehicleCategory
import qualified Database.Beam as B



data RouteT f
    = RouteT {code :: (B.C f Kernel.Prelude.Text),
              color :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
              endLat :: (B.C f Kernel.Prelude.Double),
              endLon :: (B.C f Kernel.Prelude.Double),
              id :: (B.C f Kernel.Prelude.Text),
              longName :: (B.C f Kernel.Prelude.Text),
              merchantId :: (B.C f Kernel.Prelude.Text),
              merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
              polyline :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
              roundRouteCode :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
              shortName :: (B.C f Kernel.Prelude.Text),
              startLat :: (B.C f Kernel.Prelude.Double),
              startLon :: (B.C f Kernel.Prelude.Double),
              timeBounds :: (B.C f Kernel.Types.TimeBound.TimeBound),
              vehicleType :: (B.C f Domain.Types.VehicleCategory.VehicleCategory),
              createdAt :: (B.C f Kernel.Prelude.UTCTime),
              updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table RouteT
    where data PrimaryKey RouteT f = RouteId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = RouteId . id
type Route = RouteT Identity

$(enableKVPG (''RouteT) [('id)] [[('code)]])

$(mkTableInstances (''RouteT) "route")

