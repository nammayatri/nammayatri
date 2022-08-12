module Domain.Types.FarePolicy where

import Beckn.Prelude
import Beckn.Types.Amount
import Beckn.Types.Common (HighPrecMeters, Money)
import Beckn.Types.Id (Id)
import qualified Domain.Types.Organization as Organization
import qualified Domain.Types.Vehicle.Variant as Variant

data FarePolicy = FarePolicy
  { id :: Id FarePolicy,
    organizationId :: Id Organization.Organization,
    vehicleVariant :: Variant.Variant,
    baseDistancePerKmFare :: Amount,
    baseDistanceMeters :: HighPrecMeters,
    extraKmFare :: Amount,
    deadKmFare :: Money,
    driverExtraFeeList :: [Money],
    nightShiftRate :: Maybe Double,
    nightShiftStart :: Maybe TimeOfDay,
    nightShiftEnd :: Maybe TimeOfDay,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show, Eq)

-- the formula is
-- fare = (base fare * base distance) + deadKmFare + (extraKm * extraKmFare) + driver selected extra fee
-- (and additionally night shift coefficients)

data FarePolicyAPIEntity = FarePolicyAPIEntity
  { id :: Id FarePolicy,
    vehicleVariant :: Variant.Variant,
    baseDistancePerKmFare :: Double,
    baseDistanceMeters :: HighPrecMeters,
    extraKmFare :: Double,
    deadKmFare :: Money,
    driverExtraFeeList :: [Money],
    nightShiftStart :: Maybe TimeOfDay,
    nightShiftEnd :: Maybe TimeOfDay,
    nightShiftRate :: Maybe Double
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

makeFarePolicyAPIEntity :: FarePolicy -> FarePolicyAPIEntity
makeFarePolicyAPIEntity FarePolicy {..} =
  FarePolicyAPIEntity
    { baseDistancePerKmFare = amountToDouble baseDistancePerKmFare,
      extraKmFare = amountToDouble extraKmFare,
      deadKmFare = deadKmFare,
      driverExtraFeeList = driverExtraFeeList,
      nightShiftRate = nightShiftRate,
      ..
    }
