{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FleetVehicleAssignment where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.FleetVehicleAssignment
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data FleetVehicleAssignmentT f = FleetVehicleAssignmentT
  { amount :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    assignedAt :: (B.C f Kernel.Prelude.UTCTime),
    assignmentStatus :: (B.C f Domain.Types.FleetVehicleAssignment.AssignmentStatus),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    fleetOwnerId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    id :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    ticketBookingId :: (B.C f Kernel.Prelude.Text),
    ticketBookingServiceId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    ticketPlaceId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    vehicleId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table FleetVehicleAssignmentT where
  data PrimaryKey FleetVehicleAssignmentT f = FleetVehicleAssignmentId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = FleetVehicleAssignmentId . id

type FleetVehicleAssignment = FleetVehicleAssignmentT Identity

$(enableKVPG (''FleetVehicleAssignmentT) [('id)] [[('fleetOwnerId)]])

$(mkTableInstances (''FleetVehicleAssignmentT) "fleet_vehicle_assignment")
