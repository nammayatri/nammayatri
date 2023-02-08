module Fixtures.RentalFarePolicy where

import qualified Domain.Types.FarePolicy.RentalFarePolicy as DRentalFP
import qualified Domain.Types.Vehicle as Veh
import Kernel.Prelude

defaultFarePolicy :: DRentalFP.RentalFarePolicy
defaultFarePolicy =
  DRentalFP.RentalFarePolicy
    { id = "rentalFarePolicyId",
      vehicleVariant = Veh.HATCHBACK,
      merchantId = "merchant_id",
      baseFare = 120,
      baseDistance = 100,
      baseDuration = 3,
      extraKmFare = 2,
      extraMinuteFare = 1,
      driverAllowanceForDay = Just 30,
      descriptions = []
    }
