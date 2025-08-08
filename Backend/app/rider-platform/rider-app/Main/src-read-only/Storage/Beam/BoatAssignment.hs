{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.BoatAssignment where

import qualified Database.Beam as B
import qualified Domain.Types.BoatAssignment
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data BoatAssignmentT f = BoatAssignmentT
  { assignedAt :: (B.C f Kernel.Prelude.UTCTime),
    assignedBy :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    boatNumber :: (B.C f Kernel.Prelude.Text),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    fleetOwnerId :: (B.C f Kernel.Prelude.Text),
    id :: (B.C f Kernel.Prelude.Text),
    status :: (B.C f Domain.Types.BoatAssignment.AssignmentStatus),
    ticketBookingServiceId :: (B.C f Kernel.Prelude.Text),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime),
    merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text)))
  }
  deriving (Generic, B.Beamable)

instance B.Table BoatAssignmentT where
  data PrimaryKey BoatAssignmentT f = BoatAssignmentId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = BoatAssignmentId . id

type BoatAssignment = BoatAssignmentT Identity

$(enableKVPG (''BoatAssignmentT) [('id)] [[('ticketBookingServiceId)]])

$(mkTableInstances (''BoatAssignmentT) "boat_assignment")
