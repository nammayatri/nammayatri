{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.TripTransaction where

import qualified Data.Aeson
import qualified Data.Text
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.Common
import qualified Domain.Types.TripTransaction
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data TripTransactionT f = TripTransactionT
  { allowEndingMidRoute :: B.C f Kernel.Prelude.Bool,
    conductorFleetBadgeId :: B.C f (Kernel.Prelude.Maybe Data.Text.Text),
    conductorName :: B.C f (Kernel.Prelude.Maybe Data.Text.Text),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    deviationCount :: B.C f Kernel.Prelude.Int,
    fleetBadgeId :: B.C f (Kernel.Prelude.Maybe Data.Text.Text),
    driverId :: B.C f Data.Text.Text,
    driverName :: B.C f (Kernel.Prelude.Maybe Data.Text.Text),
    dutyType :: B.C f (Kernel.Prelude.Maybe Data.Text.Text),
    endAddress :: B.C f (Kernel.Prelude.Maybe Data.Text.Text),
    endLocationLat :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    endLocationLon :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    endRideApprovalRequestId :: B.C f (Kernel.Prelude.Maybe Data.Text.Text),
    endStopCode :: B.C f Data.Text.Text,
    fleetOwnerId :: B.C f Data.Text.Text,
    id :: B.C f Data.Text.Text,
    isCurrentlyDeviated :: B.C f Kernel.Prelude.Bool,
    merchantId :: B.C f Data.Text.Text,
    merchantOperatingCityId :: B.C f Data.Text.Text,
    pilotDestinationLat :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    pilotDestinationLon :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    pilotSourceLat :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    pilotSourceLon :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    roundRouteCode :: B.C f (Kernel.Prelude.Maybe Data.Text.Text),
    routeCode :: B.C f Data.Text.Text,
    scheduledTripTime :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    startAddress :: B.C f (Kernel.Prelude.Maybe Data.Text.Text),
    startLocationLat :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    startLocationLon :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    startedNearStopCode :: B.C f (Kernel.Prelude.Maybe Data.Text.Text),
    status :: B.C f Domain.Types.TripTransaction.TripStatus,
    tripCode :: B.C f (Kernel.Prelude.Maybe Data.Text.Text),
    tripEndTime :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    tripEstimatedRouteDetails :: B.C f (Kernel.Prelude.Maybe Data.Aeson.Value),
    tripStartSource :: B.C f (Kernel.Prelude.Maybe Domain.Types.TripTransaction.ActionSource),
    tripStartTime :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    tripTerminationSource :: B.C f (Kernel.Prelude.Maybe Domain.Types.TripTransaction.ActionSource),
    tripType :: B.C f (Kernel.Prelude.Maybe Domain.Types.TripTransaction.TripType),
    updatedAt :: B.C f Kernel.Prelude.UTCTime,
    vehicleNumber :: B.C f Data.Text.Text,
    vehicleServiceTierType :: B.C f Domain.Types.Common.ServiceTierType,
    vipName :: B.C f (Kernel.Prelude.Maybe Data.Text.Text)
  }
  deriving (Generic, B.Beamable)

instance B.Table TripTransactionT where
  data PrimaryKey TripTransactionT f = TripTransactionId (B.C f Data.Text.Text) deriving (Generic, B.Beamable)
  primaryKey = TripTransactionId . id

type TripTransaction = TripTransactionT Identity

$(enableKVPG ''TripTransactionT ['id] [['driverId]])

$(mkTableInstances ''TripTransactionT "trip_transaction")
