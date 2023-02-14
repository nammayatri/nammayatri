 {-
 Copyright 2022-23, Juspay India Pvt Ltd
 
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License 
 
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program 
 
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
 
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of 
 
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.FarePolicy.OneWayFarePolicy where

import Domain.Types.Common
import Domain.Types.FarePolicy.Discount
import Domain.Types.FarePolicy.OneWayFarePolicy.PerExtraKmRate
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Vehicle as Vehicle
import Kernel.Prelude
import Kernel.Types.Common (Centesimal, Money)
import Kernel.Types.Id (Id)

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
    waitingChargePerMin :: Maybe Money,
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
