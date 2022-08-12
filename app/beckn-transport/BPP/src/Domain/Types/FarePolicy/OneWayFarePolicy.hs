module Domain.Types.FarePolicy.OneWayFarePolicy where

import Beckn.Prelude
import Beckn.Types.Common (Money)
import Beckn.Types.Id (Id)
import Domain.Types.FarePolicy.Discount
import Domain.Types.FarePolicy.OneWayFarePolicy.PerExtraKmRate
import qualified Domain.Types.Organization as Organization
import qualified Domain.Types.Vehicle as Vehicle

data OneWayFarePolicy = OneWayFarePolicy
  { id :: Id OneWayFarePolicy,
    vehicleVariant :: Vehicle.Variant,
    organizationId :: Id Organization.Organization,
    baseFare :: Maybe Money,
    perExtraKmRateList :: NonEmpty PerExtraKmRate,
    discountList :: [Discount],
    nightShiftStart :: Maybe TimeOfDay,
    nightShiftEnd :: Maybe TimeOfDay,
    nightShiftRate :: Maybe Double,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show, Eq)

data OneWayFarePolicyAPIEntity = OneWayFarePolicyAPIEntity
  { id :: Id OneWayFarePolicy,
    vehicleVariant :: Vehicle.Variant,
    baseFare :: Maybe Double,
    perExtraKmRateList :: NonEmpty PerExtraKmRateAPIEntity,
    discountList :: [DiscountAPIEntity],
    nightShiftStart :: Maybe TimeOfDay,
    nightShiftEnd :: Maybe TimeOfDay,
    nightShiftRate :: Maybe Double
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

makeOneWayFarePolicyAPIEntity :: OneWayFarePolicy -> OneWayFarePolicyAPIEntity
makeOneWayFarePolicyAPIEntity OneWayFarePolicy {..} =
  OneWayFarePolicyAPIEntity
    { id = id,
      baseFare = fromIntegral <$> baseFare,
      perExtraKmRateList = makePerExtraKmRateAPIEntity <$> perExtraKmRateList,
      discountList = makeDiscountAPIEntity <$> discountList,
      nightShiftStart = nightShiftStart,
      nightShiftEnd = nightShiftEnd,
      ..
    }
