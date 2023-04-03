{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.SlabFarePolicy where

import Domain.Types.Common
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Vehicle.Variant as Variant
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id (Id)

data SlabFarePolicyD (s :: UsageSafety) = SlabFarePolicy
  { id :: Id SlabFarePolicy,
    merchantId :: Id DM.Merchant,
    vehicleVariant :: Variant.Variant,
    serviceCharge :: Money,
    nightShiftRate :: Maybe Centesimal,
    nightShiftStart :: Maybe TimeOfDay,
    nightShiftEnd :: Maybe TimeOfDay,
    maxAllowedTripDistance :: Maybe Meters,
    minAllowedTripDistance :: Maybe Meters,
    govtChargesPerc :: Maybe Int,
    fareSlabs :: [Slab],
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show, Eq)

type SlabFarePolicy = SlabFarePolicyD 'Safe

instance FromJSON (SlabFarePolicyD 'Unsafe)

instance ToJSON (SlabFarePolicyD 'Unsafe)

data Slab = Slab
  { startMeters :: Meters,
    endMeters :: Meters,
    fare :: HighPrecMoney,
    waitingCharge :: HighPrecMoney,
    nightCharge :: HighPrecMoney
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON, ToSchema)

data SlabFarePolicyAPIEntity = SlabFarePolicyAPIEntity
  { id :: Id SlabFarePolicy,
    vehicleVariant :: Variant.Variant,
    serviceCharge :: Money,
    nightShiftStart :: Maybe TimeOfDay,
    nightShiftEnd :: Maybe TimeOfDay,
    nightShiftRate :: Maybe Centesimal,
    fareSlabs :: [Slab],
    maxAllowedTripDistance :: Maybe Meters,
    minAllowedTripDistance :: Maybe Meters
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

makeSlabFarePolicyAPIEntity :: SlabFarePolicy -> SlabFarePolicyAPIEntity
makeSlabFarePolicyAPIEntity SlabFarePolicy {..} =
  SlabFarePolicyAPIEntity
    { ..
    }
