{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.VehicleTrip where

import qualified BecknV2.FRFS.Enums
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.VehicleTrip
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data VehicleTripT f = VehicleTripT
  { capacity :: (B.C f Kernel.Prelude.Int),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    driverId :: (B.C f Kernel.Prelude.Text),
    endReason :: (B.C f (Kernel.Prelude.Maybe Domain.Types.VehicleTrip.VehicleTripEndReason)),
    endedAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    id :: (B.C f Kernel.Prelude.Text),
    integratedBppConfigId :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    missedPickups :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
    movingAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    offlineBoardings :: (B.C f Kernel.Prelude.Int),
    reachedEndAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    routeCode :: (B.C f Kernel.Prelude.Text),
    serviceTierType :: (B.C f BecknV2.FRFS.Enums.ServiceTierType),
    startedAt :: (B.C f Kernel.Prelude.UTCTime),
    status :: (B.C f Domain.Types.VehicleTrip.VehicleTripStatus),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime),
    vehicleNumber :: (B.C f Kernel.Prelude.Text)
  }
  deriving (Generic, B.Beamable)

instance B.Table VehicleTripT where
  data PrimaryKey VehicleTripT f = VehicleTripId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = VehicleTripId . id

type VehicleTrip = VehicleTripT Identity

$(enableKVPG (''VehicleTripT) [('id)] [])

$(mkTableInstances (''VehicleTripT) "vehicle_trip")
