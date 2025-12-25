{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FleetConfig where

import qualified Data.Text
import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Lib.DriverCoins.Types
import Tools.Beam.UtilsTH

data FleetConfigT f = FleetConfigT
  { allowAutomaticRoundTripAssignment :: B.C f Kernel.Prelude.Bool,
    allowEndingMidRoute :: B.C f Kernel.Prelude.Bool,
    allowStartRideFromQR :: B.C f Kernel.Prelude.Bool,
    blacklistCoinEvents :: B.C f (Kernel.Prelude.Maybe [Lib.DriverCoins.Types.DriverCoinsFunctionType]),
    directlyStartFirstTripAssignment :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    endRideDistanceThreshold :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMeters),
    fleetOwnerId :: B.C f Data.Text.Text,
    rideEndApproval :: B.C f Kernel.Prelude.Bool,
    unlinkDriverAndVehicleOnTripTermination :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    merchantId :: B.C f (Kernel.Prelude.Maybe Data.Text.Text),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Data.Text.Text),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table FleetConfigT where
  data PrimaryKey FleetConfigT f = FleetConfigId (B.C f Data.Text.Text) deriving (Generic, B.Beamable)
  primaryKey = FleetConfigId . fleetOwnerId

type FleetConfig = FleetConfigT Identity

$(enableKVPG ''FleetConfigT ['fleetOwnerId] [])

$(mkTableInstances ''FleetConfigT "fleet_config")
