module Domain.Types.FarePolicy where

import Beckn.Prelude
import Beckn.Types.Common (HighPrecMoney, Meters, Money)
import Beckn.Types.Id (Id)
import qualified Domain.Types.Organization as Organization
import qualified Domain.Types.Vehicle.Variant as Variant

data FarePolicy = FarePolicy
  { id :: Id FarePolicy,
    organizationId :: Id Organization.Organization,
    vehicleVariant :: Variant.Variant,
    baseDistanceFare :: HighPrecMoney,
    baseDistanceMeters :: Meters,
    perExtraKmFare :: HighPrecMoney,
    deadKmFare :: Money,
    driverExtraFee :: ExtraFee,
    nightShiftRate :: Maybe Double,
    nightShiftStart :: Maybe TimeOfDay,
    nightShiftEnd :: Maybe TimeOfDay,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show, Eq)

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
    nightShiftRate :: Maybe Double
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

makeFarePolicyAPIEntity :: FarePolicy -> FarePolicyAPIEntity
makeFarePolicyAPIEntity FarePolicy {..} =
  FarePolicyAPIEntity
    { ..
    }
