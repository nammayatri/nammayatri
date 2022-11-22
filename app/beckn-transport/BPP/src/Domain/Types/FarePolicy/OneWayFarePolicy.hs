module Domain.Types.FarePolicy.OneWayFarePolicy where

import Beckn.Prelude
import Beckn.Types.Common (Centesimal, Money)
import Beckn.Types.Id (Id)
import Domain.Types.Common
import Domain.Types.FarePolicy.Discount
import Domain.Types.FarePolicy.OneWayFarePolicy.PerExtraKmRate
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Vehicle as Vehicle

data OneWayFarePolicyD (s :: UsageSafety) = OneWayFarePolicy
  { id :: Id OneWayFarePolicy,
    vehicleVariant :: Vehicle.Variant,
    merchantId :: Id DM.Merchant,
    baseFare :: Maybe Money,
    perExtraKmRateList :: NonEmpty (PerExtraKmRateD s),
    discountList :: [DiscountD s],
    nightShiftStart :: Maybe TimeOfDay,
    nightShiftEnd :: Maybe TimeOfDay,
    nightShiftRate :: Maybe Centesimal,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show, Eq)

type OneWayFarePolicy = OneWayFarePolicyD 'Safe

instance FromJSON (OneWayFarePolicyD 'Unsafe)

instance ToJSON (OneWayFarePolicyD 'Unsafe)

data OneWayFarePolicyAPIEntity = OneWayFarePolicyAPIEntity
  { id :: Id OneWayFarePolicy,
    vehicleVariant :: Vehicle.Variant,
    baseFare :: Maybe Money,
    perExtraKmRateList :: NonEmpty PerExtraKmRateAPIEntity,
    discountList :: [DiscountAPIEntity],
    nightShiftStart :: Maybe TimeOfDay,
    nightShiftEnd :: Maybe TimeOfDay,
    nightShiftRate :: Maybe Centesimal
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

makeOneWayFarePolicyAPIEntity :: OneWayFarePolicy -> OneWayFarePolicyAPIEntity
makeOneWayFarePolicyAPIEntity OneWayFarePolicy {..} =
  OneWayFarePolicyAPIEntity
    { id = id,
      perExtraKmRateList = makePerExtraKmRateAPIEntity <$> perExtraKmRateList,
      discountList = makeDiscountAPIEntity <$> discountList,
      nightShiftStart = nightShiftStart,
      nightShiftEnd = nightShiftEnd,
      ..
    }
