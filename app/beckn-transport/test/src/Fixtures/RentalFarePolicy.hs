module Fixtures.RentalFarePolicy where

import Beckn.Prelude
import qualified Domain.Types.RentalFarePolicy as DRentalFP
import qualified Domain.Types.Vehicle as Veh

defaultFarePolicy :: DRentalFP.RentalFarePolicy
defaultFarePolicy =
  DRentalFP.RentalFarePolicy
    { id = "rentalFarePolicyId",
      vehicleVariant = Veh.HATCHBACK,
      organizationId = "organization_id",
      baseFare = 120.0,
      baseDistance = 100,
      baseDuration = 3,
      extraKmFare = 2,
      extraMinuteFare = 1,
      driverAllowanceForDay = Just 30
    }
