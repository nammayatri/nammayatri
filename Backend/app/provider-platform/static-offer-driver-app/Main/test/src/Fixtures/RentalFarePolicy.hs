 {-
 Copyright 2022-23, Juspay India Pvt Ltd
 
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License 
 
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program 
 
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
 
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of 
 
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

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
