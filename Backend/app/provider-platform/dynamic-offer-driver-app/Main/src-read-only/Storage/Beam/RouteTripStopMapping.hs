{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.RouteTripStopMapping where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Data.Time
import qualified Domain.Types.VehicleCategory
import qualified Database.Beam as B



data RouteTripStopMappingT f
    = RouteTripStopMappingT {enabled :: (B.C f Kernel.Prelude.Bool),
                             merchantId :: (B.C f Kernel.Prelude.Text),
                             merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
                             providerCode :: (B.C f Kernel.Prelude.Text),
                             routeCode :: (B.C f Kernel.Prelude.Text),
                             scheduledArrival :: (B.C f Data.Time.TimeOfDay),
                             scheduledDay :: (B.C f Data.Time.DayOfWeek),
                             scheduledDeparture :: (B.C f Data.Time.TimeOfDay),
                             stopCode :: (B.C f Kernel.Prelude.Text),
                             stopName :: (B.C f Kernel.Prelude.Text),
                             stopLat :: (B.C f Kernel.Prelude.Double),
                             stopLon :: (B.C f Kernel.Prelude.Double),
                             stopSequenceNum :: (B.C f Kernel.Prelude.Int),
                             tripCode :: (B.C f Kernel.Prelude.Text),
                             tripSequenceNum :: (B.C f Kernel.Prelude.Int),
                             vehicleType :: (B.C f Domain.Types.VehicleCategory.VehicleCategory),
                             createdAt :: (B.C f Kernel.Prelude.UTCTime),
                             updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table RouteTripStopMappingT
    where data PrimaryKey RouteTripStopMappingT f
              = RouteTripStopMappingId (B.C f Kernel.Prelude.Text) (B.C f Data.Time.DayOfWeek) (B.C f Kernel.Prelude.Text) (B.C f Kernel.Prelude.Text) (B.C f Kernel.Prelude.Int)
              deriving (Generic, B.Beamable)
          primaryKey = RouteTripStopMappingId <$> routeCode <*> scheduledDay <*> stopCode <*> tripCode <*> tripSequenceNum
type RouteTripStopMapping = RouteTripStopMappingT Identity

$(enableKVPG (''RouteTripStopMappingT) [('routeCode), ('scheduledDay), ('stopCode), ('tripCode), ('tripSequenceNum)] [])

$(mkTableInstances (''RouteTripStopMappingT) "route_trip_stop_mapping")

