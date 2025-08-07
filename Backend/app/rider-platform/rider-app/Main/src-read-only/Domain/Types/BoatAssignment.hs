{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.BoatAssignment where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.TicketBookingService
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data BoatAssignment = BoatAssignment
  { assignedAt :: Kernel.Prelude.UTCTime,
    assignedBy :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    boatNumber :: Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    fleetOwnerId :: Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.BoatAssignment.BoatAssignment,
    status :: Domain.Types.BoatAssignment.AssignmentStatus,
    ticketBookingServiceId :: Kernel.Types.Id.Id Domain.Types.TicketBookingService.TicketBookingService,
    updatedAt :: Kernel.Prelude.UTCTime,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity)
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data AssignmentStatus = Assigned | InUse | Completed | Cancelled deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''AssignmentStatus))
