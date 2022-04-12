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
    extraKMFare :: Amount,
    extraMinuteFare :: Amount,
    driverAllowanceForDay :: Maybe Amount
  }
  deriving (Generic, Show, Eq)
