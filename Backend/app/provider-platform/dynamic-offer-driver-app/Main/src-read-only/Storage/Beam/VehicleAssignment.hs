{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.VehicleAssignment where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.VehicleAssignment
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data VehicleAssignmentT f = VehicleAssignmentT
  { amount :: (B.C f Kernel.Types.Common.HighPrecMoney),
    assignedAt :: (B.C f Kernel.Prelude.UTCTime),
    assignmentStatus :: (B.C f Domain.Types.VehicleAssignment.AssignmentStatus),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    fleetOwnerId :: (B.C f Kernel.Prelude.Text),
    id :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    placeId :: (B.C f Kernel.Prelude.Text),
    ticketId :: (B.C f Kernel.Prelude.Text),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime),
    vehicleId :: (B.C f Kernel.Prelude.Text)
  }
  deriving (Generic, B.Beamable)

instance B.Table VehicleAssignmentT where
  data PrimaryKey VehicleAssignmentT f = VehicleAssignmentId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = VehicleAssignmentId . id

type VehicleAssignment = VehicleAssignmentT Identity

$(enableKVPG (''VehicleAssignmentT) [('id)] [[('fleetOwnerId)], [('ticketId)]])

$(mkTableInstances (''VehicleAssignmentT) "vehicle_assignment")
