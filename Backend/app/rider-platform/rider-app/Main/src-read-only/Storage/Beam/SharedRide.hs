{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.SharedRide where

import qualified Data.Aeson
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.ServiceTierType
import qualified Domain.Types.SharedRide
import qualified Domain.Types.VehicleVariant
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data SharedRideT f = SharedRideT
  { bppSharedRideId :: (B.C f Kernel.Prelude.Text),
    chargeableDistanceValue :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecDistance)),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    driverId :: (B.C f Kernel.Prelude.Text),
    id :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    rideEndTime :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    rideIds :: (B.C f [Kernel.Prelude.Text]),
    rideStartTime :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    sharedBookingId :: (B.C f Kernel.Prelude.Text),
    status :: (B.C f Domain.Types.SharedRide.SharedRideStatus),
    totalFare :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    trackingUrl :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    traveledDistanceValue :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecDistance)),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime),
    vehicleModel :: (B.C f Kernel.Prelude.Text),
    vehicleNumber :: (B.C f Kernel.Prelude.Text),
    vehicleServiceTierType :: (B.C f (Kernel.Prelude.Maybe Domain.Types.ServiceTierType.ServiceTierType)),
    vehicleVariant :: (B.C f Domain.Types.VehicleVariant.VehicleVariant),
    waypoints :: (B.C f Data.Aeson.Value)
  }
  deriving (Generic, B.Beamable)

instance B.Table SharedRideT where
  data PrimaryKey SharedRideT f = SharedRideId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = SharedRideId . id

type SharedRide = SharedRideT Identity

$(enableKVPG (''SharedRideT) [('id)] [[('sharedBookingId)]])

$(mkTableInstances (''SharedRideT) "shared_ride")
