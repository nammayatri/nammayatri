module Domain.Types.FarePolicy where

import Beckn.Prelude
import Beckn.Types.Common (Centesimal, HighPrecMoney, Meters, Money)
import Beckn.Types.Id (Id)
import Domain.Types.Common
import qualified Domain.Types.Organization as Organization
import qualified Domain.Types.Vehicle.Variant as Variant

data FarePolicyD s = FarePolicy
  { id :: Id FarePolicy,
    organizationId :: Id Organization.Organization,
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
