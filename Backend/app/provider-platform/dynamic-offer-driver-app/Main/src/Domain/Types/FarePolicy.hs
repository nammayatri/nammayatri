{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.FarePolicy where

import qualified Data.List.NonEmpty as NE
import Data.Ord
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
    driverExtraFeeBounds :: Maybe DriverExtraFeeBounds,
    serviceCharge :: Maybe Money,
    nightShiftBounds :: Maybe NightShiftBounds,
    allowedTripDistanceBounds :: Maybe AllowedTripDistanceBounds,
    waitingTimeEstimatedThreshold :: Maybe Seconds, -- What is this field?
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

data FPProgressiveDetailsD (s :: UsageSafety) = FPProgressiveDetails
  { baseFare :: Money,
    baseDistance :: Meters,
    perExtraKmFare :: HighPrecMoney,
    deadKmFare :: Money,
    waitingCharge :: Maybe WaitingCharge,
    nightShiftCharge :: Maybe NightShiftCharge
  }
  deriving (Generic, Show, Eq)

type FPProgressiveDetails = FPProgressiveDetailsD 'Safe

instance FromJSON (FPProgressiveDetailsD 'Unsafe)

instance ToJSON (FPProgressiveDetailsD 'Unsafe)

newtype FPSlabsDetailsD (s :: UsageSafety) = FPSlabsDetails
  { slabs :: NonEmpty (FPSlabsDetailsSlabD s)
  }
  deriving (Generic, Show, Eq)

type FPSlabsDetails = FPSlabsDetailsD 'Safe

instance FromJSON (FPSlabsDetailsD 'Unsafe)

instance ToJSON (FPSlabsDetailsD 'Unsafe)

findFPSlabsDetailsSlabByDistance :: Meters -> NonEmpty (FPSlabsDetailsSlabD s) -> FPSlabsDetailsSlabD s
findFPSlabsDetailsSlabByDistance dist slabList = do
  case NE.filter (\slab -> slab.startDistance < dist) $ NE.sortBy (comparing (.startDistance)) slabList of
    [] -> error $ "Slab for dist = " <> show dist <> " not found. Non-emptiness supposed to be guaranteed by app logic."
    a -> last a

data FPSlabsDetailsSlabD (s :: UsageSafety) = FPSlabsDetailsSlab
  { startDistance :: Meters,
    baseFare :: Money,
    waitingCharge :: Maybe WaitingCharge,
    nightShiftCharge :: Maybe NightShiftCharge
  }
  deriving (Generic, Show, Eq, ToSchema)

type FPSlabsDetailsSlab = FPSlabsDetailsSlabD 'Safe

instance FromJSON (FPSlabsDetailsSlabD 'Unsafe)

instance ToJSON (FPSlabsDetailsSlabD 'Unsafe)

data DriverExtraFeeBounds = DriverExtraFeeBounds
  { minFee :: Money,
    maxFee :: Money
  }
  deriving (Generic, Eq, Show, ToJSON, FromJSON, ToSchema)

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

data WaitingCharge = PerMinuteWaitingCharge HighPrecMoney | ConstantWaitingCharge Money
  deriving (Generic, Eq, Show, ToJSON, FromJSON, ToSchema)

data NightShiftCharge = ProgressiveNightShiftCharge Float | ConstantNightShiftCharge Money
  deriving (Generic, Eq, Show, ToJSON, FromJSON, ToSchema)

-----------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------APIEntity--------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------

data FarePolicyAPIEntity = FarePolicyAPIEntity
  { id :: Id FarePolicy,
    vehicleVariant :: Variant.Variant,
    driverExtraFeeBounds :: Maybe DriverExtraFeeBounds,
    serviceCharge :: Maybe Money,
    nightShiftBounds :: Maybe NightShiftBounds,
    allowedTripDistanceBounds :: Maybe AllowedTripDistanceBounds,
    waitingTimeEstimatedThreshold :: Maybe Seconds,
    govtCharges :: Maybe Double,
    farePolicyDetails :: FarePolicyDetailsAPIEntity
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data FarePolicyDetailsAPIEntity = ProgressiveDetailsAPIEntity FPProgressiveDetailsAPIEntity | SlabsDetailsAPIEntity FPSlabsDetailsAPIEntity
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data FPProgressiveDetailsAPIEntity = FPProgressiveDetailsAPIEntity
  { baseFare :: Money,
    baseDistance :: Meters,
    perExtraKmFare :: HighPrecMoney,
    deadKmFare :: Money,
    waitingCharge :: Maybe WaitingCharge,
    nightShiftCharge :: Maybe NightShiftCharge
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

newtype FPSlabsDetailsAPIEntity = FPSlabsDetailsAPIEntity
  { slabs :: NonEmpty FPSlabsDetailsSlabAPIEntity
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data FPSlabsDetailsSlabAPIEntity = FPSlabsDetailsSlabAPIEntity
  { startDistance :: Meters,
    baseFare :: Money,
    waitingCharge :: Maybe WaitingCharge,
    nightShiftCharge :: Maybe NightShiftCharge
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON, ToSchema)

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

    makeFPProgressiveDetailsAPIEntity :: FPProgressiveDetails -> FPProgressiveDetailsAPIEntity
    makeFPProgressiveDetailsAPIEntity FPProgressiveDetails {..} =
      FPProgressiveDetailsAPIEntity
        { ..
        }

    makeFPSlabsDetailsAPIEntity :: FPSlabsDetails -> FPSlabsDetailsAPIEntity
    makeFPSlabsDetailsAPIEntity FPSlabsDetails {..} =
      FPSlabsDetailsAPIEntity
        { slabs = makeFPSlabsDetailsSlabAPIEntity <$> slabs
        }

    makeFPSlabsDetailsSlabAPIEntity :: FPSlabsDetailsSlab -> FPSlabsDetailsSlabAPIEntity
    makeFPSlabsDetailsSlabAPIEntity FPSlabsDetailsSlab {..} =
      FPSlabsDetailsSlabAPIEntity
        { ..
        }
