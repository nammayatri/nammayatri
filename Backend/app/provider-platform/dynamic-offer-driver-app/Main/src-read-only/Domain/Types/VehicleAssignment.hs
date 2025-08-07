{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.VehicleAssignment where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.Vehicle
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data VehicleAssignment = VehicleAssignment
  { amount :: Kernel.Types.Common.HighPrecMoney,
    assignedAt :: Kernel.Prelude.UTCTime,
    assignmentStatus :: Domain.Types.VehicleAssignment.AssignmentStatus,
    createdAt :: Kernel.Prelude.UTCTime,
    fleetOwnerId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    id :: Kernel.Types.Id.Id Domain.Types.VehicleAssignment.VehicleAssignment,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    placeId :: Kernel.Prelude.Text,
    ticketId :: Kernel.Prelude.Text,
    updatedAt :: Kernel.Prelude.UTCTime,
    vehicleId :: Kernel.Types.Id.Id Domain.Types.Vehicle.Vehicle
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data AssignmentStatus = ASSIGNED | COMPLETED | CANCELLED deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, Show, Eq, Ord)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''AssignmentStatus))
