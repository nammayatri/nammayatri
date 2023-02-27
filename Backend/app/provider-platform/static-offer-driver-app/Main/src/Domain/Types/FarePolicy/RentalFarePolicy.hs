{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.FarePolicy.RentalFarePolicy where

import Domain.Types.Common
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Vehicle as Vehicle
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id (Id)

data RentalFarePolicyD (s :: UsageSafety) = RentalFarePolicy
  { id :: Id RentalFarePolicy,
    merchantId :: Id DM.Merchant,
    vehicleVariant :: Vehicle.Variant,
    baseFare :: Money,
    baseDistance :: Kilometers,
    baseDuration :: Hours,
    extraKmFare :: HighPrecMoney,
    extraMinuteFare :: HighPrecMoney,
    driverAllowanceForDay :: Maybe Money,
    descriptions :: [Text]
  }
  deriving (Generic, Show, Eq)

type RentalFarePolicy = RentalFarePolicyD 'Safe

instance FromJSON (RentalFarePolicyD 'Unsafe)

instance ToJSON (RentalFarePolicyD 'Unsafe)

data RentalFarePolicyAPIEntity = RentalFarePolicyAPIEntity
  { id :: Id RentalFarePolicy,
    vehicleVariant :: Vehicle.Variant,
    baseFare :: Money,
    baseDistance :: Kilometers,
    baseDuration :: Hours,
    extraKmFare :: HighPrecMoney,
    extraMinuteFare :: HighPrecMoney,
    driverAllowanceForDay :: Maybe Money
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

mkDescriptions :: HighPrecMoney -> HighPrecMoney -> Maybe Money -> [Text]
mkDescriptions kmRate minuteRate minuteFare =
  [ "Extra km fare: " <> show kmRate,
    "Extra min fare: " <> show minuteRate,
    "Extra fare for day: " <> maybe "not allowed" show minuteFare,
    "A rider can choose this package for a trip where the rider may not have a pre-decided destination and may not want to return to the origin location",
    "The rider may want to stop at multiple destinations and have the taxi wait for the rider at these locations"
  ]

makeRentalFarePolicyAPIEntity :: RentalFarePolicy -> RentalFarePolicyAPIEntity
makeRentalFarePolicyAPIEntity RentalFarePolicy {..} = RentalFarePolicyAPIEntity {..}
