{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.FleetVehicleAssignment where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.Vehicle
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data FleetVehicleAssignment = FleetVehicleAssignment
  { amount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    assignedAt :: Kernel.Prelude.UTCTime,
    assignmentStatus :: Domain.Types.FleetVehicleAssignment.AssignmentStatus,
    createdAt :: Kernel.Prelude.UTCTime,
    fleetOwnerId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    id :: Kernel.Types.Id.Id Domain.Types.FleetVehicleAssignment.FleetVehicleAssignment,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    ticketBookingId :: Kernel.Prelude.Text,
    ticketBookingServiceId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    ticketPlaceId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Vehicle.Vehicle),
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data AssignmentStatus = NEW | ASSIGNED | COMPLETED | CANCELLED deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''AssignmentStatus))
