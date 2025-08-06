{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.SharedRideConfigs where

import qualified BecknV2.OnDemand.Enums
import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data SharedRideConfigsT f = SharedRideConfigsT
  { actualDropDistanceThreshold :: B.C f Kernel.Types.Common.Meters,
    actualPickupDistanceThreshold :: B.C f Kernel.Types.Common.Meters,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    customerRemainingThresholdForFlowContinuation :: B.C f Kernel.Prelude.Int,
    dropLocationSearchRadius :: B.C f Kernel.Types.Common.Meters,
    enableSharedRide :: B.C f Kernel.Prelude.Bool,
    enableSyncPooling :: B.C f Kernel.Prelude.Bool,
    geoHashPrecisionForRouteMatching :: B.C f Kernel.Prelude.Int,
    id :: B.C f Kernel.Prelude.Text,
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    pickupLocationSearchRadius :: B.C f Kernel.Types.Common.Meters,
    routeMatchingThreshold :: B.C f Kernel.Prelude.Double,
    routeOverlapThreshold :: B.C f Kernel.Prelude.Double,
    searchExpiryBufferSeconds :: B.C f Kernel.Types.Common.Seconds,
    searchRequestExpirySeconds :: B.C f Kernel.Types.Common.Seconds,
    searchThresholdForSharedEstimate :: B.C f Kernel.Prelude.Int,
    updatedAt :: B.C f Kernel.Prelude.UTCTime,
    vehicleCategory :: B.C f BecknV2.OnDemand.Enums.VehicleCategory
  }
  deriving (Generic, B.Beamable)

instance B.Table SharedRideConfigsT where
  data PrimaryKey SharedRideConfigsT f = SharedRideConfigsId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = SharedRideConfigsId . id

type SharedRideConfigs = SharedRideConfigsT Identity

$(enableKVPG ''SharedRideConfigsT ['id] [])

$(mkTableInstances ''SharedRideConfigsT "shared_ride_configs")
