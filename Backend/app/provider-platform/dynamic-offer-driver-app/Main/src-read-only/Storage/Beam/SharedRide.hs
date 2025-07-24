{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.SharedRide where

import qualified Data.Aeson
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.Common
import qualified Domain.Types.Ride
import qualified Domain.Types.ServiceTierType
import qualified Domain.Types.SharedRide
import qualified Domain.Types.VehicleVariant
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data SharedRideT f = SharedRideT
  { chargeableDistance :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Meters)),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    driverId :: (B.C f Kernel.Prelude.Text),
    estimatedTollCharges :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    estimatedTollNames :: (B.C f (Kernel.Prelude.Maybe [Kernel.Prelude.Text])),
    id :: (B.C f Kernel.Prelude.Text),
    isPickupOrDestinationEdited :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    numberOfDeviation :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)),
    numberOfOsrmSnapToRoadCalls :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
    numberOfSelfTuned :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
    numberOfSnapToRoadCalls :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
    passedThroughDestination :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)),
    rideEndTime :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    rideEndedBy :: (B.C f (Kernel.Prelude.Maybe Domain.Types.Ride.RideEndedBy)),
    rideIds :: (B.C f [Kernel.Prelude.Text]),
    rideStartTime :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    sharedBookingId :: (B.C f Kernel.Prelude.Text),
    status :: (B.C f Domain.Types.SharedRide.SharedRideStatus),
    tollCharges :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    tollNames :: (B.C f (Kernel.Prelude.Maybe [Kernel.Prelude.Text])),
    totalFare :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    trackingUrl :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    transactionId :: (B.C f Kernel.Prelude.Text),
    traveledDistance :: (B.C f Kernel.Types.Common.HighPrecMeters),
    tripCategory :: (B.C f Domain.Types.Common.TripCategory),
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
