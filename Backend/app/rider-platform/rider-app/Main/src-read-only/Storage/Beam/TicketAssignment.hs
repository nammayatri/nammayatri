{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.TicketAssignment where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.TicketAssignment
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data TicketAssignmentT f = TicketAssignmentT
  { assignedBy :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    assignmentNumber :: (B.C f Kernel.Prelude.Text),
    assignmentType :: (B.C f Domain.Types.TicketAssignment.AssignmentType),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    id :: (B.C f Kernel.Prelude.Text),
    status :: (B.C f Domain.Types.TicketAssignment.AssignmentStatus),
    ticketBookingId :: (B.C f Kernel.Prelude.Text),
    ticketBookingServiceId :: (B.C f Kernel.Prelude.Text),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime),
    merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text)))
  }
  deriving (Generic, B.Beamable)

instance B.Table TicketAssignmentT where
  data PrimaryKey TicketAssignmentT f = TicketAssignmentId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = TicketAssignmentId . id

type TicketAssignment = TicketAssignmentT Identity

$(enableKVPG (''TicketAssignmentT) [('id)] [[('ticketBookingId)], [('ticketBookingServiceId)]])

$(mkTableInstances (''TicketAssignmentT) "ticket_assignment")
