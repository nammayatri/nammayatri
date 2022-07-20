module Domain.Types.FarePolicy.OneWayFarePolicy where

import Beckn.Prelude
import Beckn.Types.Id (Id)
import Domain.Types.FarePolicy.Discount
import Domain.Types.FarePolicy.OneWayFarePolicy.PerExtraKmRate
import qualified Domain.Types.Organization as Organization
import qualified Domain.Types.Vehicle as Vehicle

data FarePolicy = FarePolicy
  { id :: Id FarePolicy,
    vehicleVariant :: Vehicle.Variant,
    organizationId :: Id Organization.Organization,
    baseFare :: Maybe Rational,
    perExtraKmRateList :: NonEmpty PerExtraKmRate,
    discountList :: [Discount],
    nightShiftStart :: Maybe TimeOfDay,
    nightShiftEnd :: Maybe TimeOfDay,
    nightShiftRate :: Maybe Rational,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show, Eq)

data FarePolicyAPIEntity = FarePolicyAPIEntity
  { id :: Id FarePolicy,
    vehicleVariant :: Vehicle.Variant,
    baseFare :: Maybe Double,
    perExtraKmRateList :: NonEmpty PerExtraKmRateAPIEntity,
    discountList :: [DiscountAPIEntity],
    nightShiftStart :: Maybe TimeOfDay,
    nightShiftEnd :: Maybe TimeOfDay,
    nightShiftRate :: Maybe Double
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

makeFarePolicyAPIEntity :: FarePolicy -> FarePolicyAPIEntity
makeFarePolicyAPIEntity FarePolicy {..} =
  FarePolicyAPIEntity
    { id = id,
      baseFare = fromRational <$> baseFare,
      perExtraKmRateList = makePerExtraKmRateAPIEntity <$> perExtraKmRateList,
      discountList = makeDiscountAPIEntity <$> discountList,
      nightShiftStart = nightShiftStart,
      nightShiftEnd = nightShiftEnd,
      nightShiftRate = fromRational <$> nightShiftRate,
      ..
    }
