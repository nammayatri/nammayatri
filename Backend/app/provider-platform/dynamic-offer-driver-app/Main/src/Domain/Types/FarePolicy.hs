{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Domain.Types.FarePolicy (module Reexport, module Domain.Types.FarePolicy) where

import Domain.Types.Common
import Domain.Types.FarePolicy.DriverExtraFeeBounds as Reexport
import Domain.Types.FarePolicy.FarePolicyProgressiveDetails as Reexport
import Domain.Types.FarePolicy.FarePolicyRentalSlabsDetails as Reexport
import Domain.Types.FarePolicy.FarePolicySlabsDetails as Reexport
import Domain.Types.Merchant
import Domain.Types.Vehicle.Variant
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id (Id)
import Tools.Beam.UtilsTH (mkBeamInstancesForEnum)

data FarePolicyD (s :: UsageSafety) = FarePolicy
  { id :: Id FarePolicy,
    driverExtraFeeBounds :: Maybe (NonEmpty DriverExtraFeeBounds),
    serviceCharge :: Maybe Money,
    nightShiftBounds :: Maybe NightShiftBounds,
    allowedTripDistanceBounds :: Maybe AllowedTripDistanceBounds,
    govtCharges :: Maybe Double,
    farePolicyDetails :: FarePolicyDetailsD s,
    description :: Maybe Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic)

type FarePolicy = FarePolicyD 'Safe

instance FromJSON (FarePolicyD 'Unsafe)

instance ToJSON (FarePolicyD 'Unsafe)

data FarePolicyDetailsD (s :: UsageSafety) = ProgressiveDetails (FPProgressiveDetailsD s) | SlabsDetails (FPSlabsDetailsD s) | RentalSlabDetails (FPRSlabDetailsD s)
  deriving (Generic, Show)

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

data FarePolicyType = Progressive | Slabs | Rental
  deriving stock (Show, Eq, Read, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

$(mkBeamInstancesForEnum ''FarePolicyType)

getFarePolicyType :: FarePolicy -> FarePolicyType
getFarePolicyType farePolicy = case farePolicy.farePolicyDetails of
  ProgressiveDetails _ -> Progressive
  SlabsDetails _ -> Slabs
  RentalSlabDetails _ -> Rental

data FullFarePolicy = FullFarePolicy
  { id :: Id FarePolicy,
    merchantId :: Id Merchant,
    vehicleVariant :: Variant,
    driverExtraFeeBounds :: Maybe (NonEmpty DriverExtraFeeBounds),
    serviceCharge :: Maybe Money,
    nightShiftBounds :: Maybe NightShiftBounds,
    allowedTripDistanceBounds :: Maybe AllowedTripDistanceBounds,
    govtCharges :: Maybe Double,
    farePolicyDetails :: FarePolicyDetails,
    description :: Maybe Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show)

farePolicyToFullFarePolicy :: Id Merchant -> Variant -> FarePolicy -> FullFarePolicy
farePolicyToFullFarePolicy merchantId vehicleVariant FarePolicy {..} =
  FullFarePolicy {..}

type FullDriverExtraFeeBounds = (Id FarePolicy, DriverExtraFeeBounds)

type FullFarePolicyProgressiveDetails = (Id FarePolicy, FPProgressiveDetails)
