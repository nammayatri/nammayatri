{-
 Copyright 2022-23, Juspay India Pvt Ltd
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.FarePolicy.FarePolicyRentalDetails
  ( module Reexport,
    module Domain.Types.FarePolicy.FarePolicyRentalDetails,
  )
where

import Domain.Types.Common
import Domain.Types.FarePolicy.Common as Reexport
import Domain.Types.FarePolicy.FarePolicyRentalDetails.FarePolicyRentalDetailsDistanceBuffer as Reexport
import Kernel.Prelude
import Kernel.Types.Common

data FPRentalDetailsD (s :: UsageSafety) = FPRentalDetails
  { baseFare :: Money,
    perHourCharge :: Money,
    distanceBuffers :: NonEmpty (FPRentalDetailsDistanceBuffersD s),
    perExtraKmRate :: Money,
    perExtraMinRate :: Money,
    includedKmPerHr :: Kilometers,
    plannedPerKmRate :: Money,
    nightShiftCharge :: Maybe NightShiftCharge
  }
  deriving (Generic, Show)

type FPRentalDetails = FPRentalDetailsD 'Safe

instance FromJSON (FPRentalDetailsD 'Unsafe)

instance ToJSON (FPRentalDetailsD 'Unsafe)
