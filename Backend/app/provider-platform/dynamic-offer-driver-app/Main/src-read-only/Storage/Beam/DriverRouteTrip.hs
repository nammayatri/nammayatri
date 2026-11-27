{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.DriverRouteTrip where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.DriverRouteTrip
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data DriverRouteTripT f = DriverRouteTripT
  { createdAt :: (B.C f Kernel.Prelude.UTCTime),
    driverId :: (B.C f Kernel.Prelude.Text),
    id :: (B.C f Kernel.Prelude.Text),
    idempotencyKey :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    rideId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    routeCode :: (B.C f Kernel.Prelude.Text),
    source :: (B.C f Domain.Types.DriverRouteTrip.DriverRouteTripSource),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime),
    vehicleNumber :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text))
  }
  deriving (Generic, B.Beamable)

instance B.Table DriverRouteTripT where
  data PrimaryKey DriverRouteTripT f = DriverRouteTripId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = DriverRouteTripId . id

type DriverRouteTrip = DriverRouteTripT Identity

$(enableKVPG (''DriverRouteTripT) [('id)] [[('driverId)], [('idempotencyKey)]])

$(mkTableInstances (''DriverRouteTripT) "driver_route_trip")
