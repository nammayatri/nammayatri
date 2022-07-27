module Domain.Types.FarePolicy where

import Beckn.Prelude
import Beckn.Types.Amount
import Beckn.Types.Common (HighPrecMeters)
import Beckn.Types.Id (Id)
import qualified Domain.Types.Organization as Organization
import qualified Domain.Types.Vehicle.Variant as Variant

data FarePolicy = FarePolicy
  { id :: Id FarePolicy,
    organizationId :: Id Organization.Organization,
    vehicleVariant :: Variant.Variant,
    baseDistancePerKmFare :: Amount,
    baseDistance :: HighPrecMeters,
    extraKmFare :: Amount,
    deadKmFare :: Amount,
    driverExtraFeeList :: [Amount],
    nightShiftRate :: Maybe Amount,
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
    baseDistance :: HighPrecMeters,
    extraKmFare :: Double,
    deadKmFare :: Double,
    driverExtraFeeList :: [Double],
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
      deadKmFare = amountToDouble deadKmFare,
      driverExtraFeeList = map amountToDouble driverExtraFeeList,
      nightShiftRate = amountToDouble <$> nightShiftRate,
      ..
    }
