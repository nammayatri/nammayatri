module Domain.Types.RentalFarePolicy where

import Beckn.Prelude
import Beckn.Types.Amount
import Beckn.Types.Id (Id)
import qualified Domain.Types.Organization as Organization
import qualified Domain.Types.Vehicle as Vehicle

data RentalFarePolicy = RentalFarePolicy
  { id :: Id RentalFarePolicy,
    organizationId :: Id Organization.Organization,
    vehicleVariant :: Vehicle.Variant,
    baseFare :: Amount,
    baseDistance :: Double, -- Distance
    baseDurationHr :: Int,
    extraKmFare :: Amount,
    extraMinuteFare :: Amount,
    driverAllowanceForDay :: Maybe Amount
  }
  deriving (Generic, Show, Eq)

data RentalFarePolicyAPIEntity = RentalFarePolicyAPIEntity
  { id :: Id RentalFarePolicy,
    vehicleVariant :: Vehicle.Variant,
    baseFare :: Amount,
    baseDistance :: Double, -- Distance
    baseDurationHr :: Int,
    extraKmFare :: Amount,
    extraMinuteFare :: Amount,
    driverAllowanceForDay :: Maybe Amount
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

makeRentalFarePolicyAPIEntity :: RentalFarePolicy -> RentalFarePolicyAPIEntity
makeRentalFarePolicyAPIEntity RentalFarePolicy {..} = RentalFarePolicyAPIEntity {..}
