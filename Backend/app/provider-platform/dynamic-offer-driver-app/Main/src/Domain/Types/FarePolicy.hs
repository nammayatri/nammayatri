{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.FarePolicy (module Reexport, module Domain.Types.FarePolicy) where

import Domain.Types.Common
import Domain.Types.FarePolicy.DriverExtraFeeBounds as Reexport
import Domain.Types.FarePolicy.FarePolicyProgressiveDetails as Reexport
import Domain.Types.FarePolicy.FarePolicySlabsDetails as Reexport
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Vehicle.Variant as Variant
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id (Id)

data FarePolicyD (s :: UsageSafety) = FarePolicy
  { id :: Id FarePolicy,
    merchantId :: Id DM.Merchant,
    vehicleVariant :: Variant.Variant,
    driverExtraFeeBounds :: Maybe (NonEmpty DriverExtraFeeBounds),
    serviceCharge :: Maybe Money,
    nightShiftBounds :: Maybe NightShiftBounds,
    allowedTripDistanceBounds :: Maybe AllowedTripDistanceBounds,
    govtCharges :: Maybe Double,
    farePolicyDetails :: FarePolicyDetailsD s,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show, Eq)

type FarePolicy = FarePolicyD 'Safe

instance FromJSON (FarePolicyD 'Unsafe)

instance ToJSON (FarePolicyD 'Unsafe)

data FarePolicyDetailsD (s :: UsageSafety) = ProgressiveDetails (FPProgressiveDetailsD s) | SlabsDetails (FPSlabsDetailsD s)
  deriving (Generic, Show, Eq)

type FarePolicyDetails = FarePolicyDetailsD 'Safe

instance FromJSON (FarePolicyDetailsD 'Unsafe)

instance ToJSON (FarePolicyDetailsD 'Unsafe)

data NightShiftBounds = NightShiftBounds
  { nightShiftStart :: TimeOfDay,
    nightShiftEnd :: TimeOfDay
  }
  deriving (Generic, Eq, Show, ToJSON, FromJSON, ToSchema)

data AllowedTripDistanceBounds = AllowedTripDistanceBounds
  { maxAllowedTripDistance :: Meters,
    minAllowedTripDistance :: Meters
  }
  deriving (Generic, Eq, Show, ToJSON, FromJSON, ToSchema)

data FarePolicyType = Progressive | Slabs deriving (Show, Read)

getFarePolicyType :: FarePolicy -> FarePolicyType
getFarePolicyType farePolicy = case farePolicy.farePolicyDetails of
  ProgressiveDetails _ -> Progressive
  SlabsDetails _ -> Slabs

-----------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------APIEntity--------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------

data FarePolicyAPIEntity = FarePolicyAPIEntity
  { id :: Id FarePolicy,
    vehicleVariant :: Variant.Variant,
    driverExtraFeeBounds :: Maybe (NonEmpty DriverExtraFeeBounds),
    serviceCharge :: Maybe Money,
    nightShiftBounds :: Maybe NightShiftBounds,
    allowedTripDistanceBounds :: Maybe AllowedTripDistanceBounds,
    govtCharges :: Maybe Double,
    farePolicyDetails :: FarePolicyDetailsAPIEntity
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data FarePolicyDetailsAPIEntity = ProgressiveDetailsAPIEntity FPProgressiveDetailsAPIEntity | SlabsDetailsAPIEntity FPSlabsDetailsAPIEntity
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

makeFarePolicyAPIEntity :: FarePolicy -> FarePolicyAPIEntity
makeFarePolicyAPIEntity FarePolicy {..} =
  FarePolicyAPIEntity
    { farePolicyDetails = makeFarePolicyDetailsAPIEntity farePolicyDetails,
      ..
    }
  where
    makeFarePolicyDetailsAPIEntity :: FarePolicyDetails -> FarePolicyDetailsAPIEntity
    makeFarePolicyDetailsAPIEntity = \case
      ProgressiveDetails det -> ProgressiveDetailsAPIEntity $ makeFPProgressiveDetailsAPIEntity det
      SlabsDetails det -> SlabsDetailsAPIEntity $ makeFPSlabsDetailsAPIEntity det
