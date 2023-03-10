{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.FarePolicy where

import Domain.Types.Common
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Vehicle.Variant as Variant
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id (Id)

data FarePolicyD (s :: UsageSafety) = FarePolicy
  { id :: Id FarePolicy,
    merchantId :: Id DM.Merchant,
    vehicleVariant :: Variant.Variant,
    baseDistanceFare :: HighPrecMoney,
    baseDistanceMeters :: Meters,
    perExtraKmFare :: HighPrecMoney,
    deadKmFare :: Money,
    driverExtraFee :: ExtraFee,
    nightShiftRate :: Maybe Centesimal,
    nightShiftStart :: Maybe TimeOfDay,
    nightShiftEnd :: Maybe TimeOfDay,
    maxAllowedTripDistance :: Maybe Meters,
    minAllowedTripDistance :: Maybe Meters,
    waitingChargePerMin :: Maybe Money,
    waitingTimeEstimatedThreshold :: Maybe Seconds,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show, Eq)

type FarePolicy = FarePolicyD 'Safe

instance FromJSON (FarePolicyD 'Unsafe)

instance ToJSON (FarePolicyD 'Unsafe)

data ExtraFee = ExtraFee
  { minFee :: Money,
    maxFee :: Money
  }
  deriving (Generic, Eq, Show, ToJSON, FromJSON, ToSchema)

-- the formula is
-- fare = (base fare * base distance) + deadKmFare + (extraKm * extraKmFare) + driver selected extra fee
-- (and additionally night shift coefficients)

data FarePolicyAPIEntity = FarePolicyAPIEntity
  { id :: Id FarePolicy,
    vehicleVariant :: Variant.Variant,
    baseDistanceFare :: HighPrecMoney,
    baseDistanceMeters :: Meters,
    perExtraKmFare :: HighPrecMoney,
    deadKmFare :: Money,
    driverExtraFee :: ExtraFee,
    nightShiftStart :: Maybe TimeOfDay,
    nightShiftEnd :: Maybe TimeOfDay,
    nightShiftRate :: Maybe Centesimal,
    maxAllowedTripDistance :: Maybe Meters,
    minAllowedTripDistance :: Maybe Meters
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

makeFarePolicyAPIEntity :: FarePolicy -> FarePolicyAPIEntity
makeFarePolicyAPIEntity FarePolicy {..} =
  FarePolicyAPIEntity
    { ..
    }
