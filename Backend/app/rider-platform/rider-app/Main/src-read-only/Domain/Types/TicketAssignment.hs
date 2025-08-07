{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.TicketAssignment where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.TicketBooking
import qualified Domain.Types.TicketBookingService
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data TicketAssignment = TicketAssignment
  { assignedBy :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    assignmentNumber :: Kernel.Prelude.Text,
    assignmentType :: Domain.Types.TicketAssignment.AssignmentType,
    createdAt :: Kernel.Prelude.UTCTime,
    id :: Kernel.Types.Id.Id Domain.Types.TicketAssignment.TicketAssignment,
    status :: Domain.Types.TicketAssignment.AssignmentStatus,
    ticketBookingId :: Kernel.Types.Id.Id Domain.Types.TicketBooking.TicketBooking,
    ticketBookingServiceId :: Kernel.Types.Id.Id Domain.Types.TicketBookingService.TicketBookingService,
    updatedAt :: Kernel.Prelude.UTCTime,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity)
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data AssignmentStatus = New | Assigned | InUse | Completed | Cancelled | Expired deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data AssignmentType = Boat | Vehicle | Seat | Other deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''AssignmentStatus))

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''AssignmentType))
