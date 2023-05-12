{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.Estimate where

import Domain.Types.Common (UsageSafety (..))
import qualified Domain.Types.Vehicle as Variant
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id

data Estimate = Estimate
  { id :: Id Estimate,
    transactionId :: Text,
    vehicleVariant :: Variant.Variant,
    minFare :: Money,
    maxFare :: Money,
    estimateBreakupList :: [EstimateBreakup],
    nightShiftInfo :: Maybe NightShiftInfo,
    waitingCharges :: WaitingCharges,
    createdAt :: UTCTime
  }
  deriving (Generic)

data WaitingCharges = WaitingCharges
  { waitingChargePerMin :: Maybe Money,
    waitingOrPickupCharges :: Maybe Money
  }
  deriving (Generic)

data NightShiftInfo = NightShiftInfo
  { nightShiftCharge :: Money,
    oldNightShiftCharge :: Centesimal, -- TODO: Doesn't make sense, to be removed
    nightShiftStart :: TimeOfDay,
    nightShiftEnd :: TimeOfDay
  }
  deriving (Generic)

data EstimateBreakupD (s :: UsageSafety) = EstimateBreakup
  { title :: Text,
    price :: EstimateBreakupPriceD s
  }
  deriving (Generic)

type EstimateBreakup = EstimateBreakupD 'Safe

deriving instance FromJSON (EstimateBreakupD 'Unsafe)

deriving instance ToJSON (EstimateBreakupD 'Unsafe)

data EstimateBreakupPriceD (s :: UsageSafety) = EstimateBreakupPrice
  { currency :: Text,
    value :: Money
  }
  deriving (Generic)

type EstimateBreakupPrice = EstimateBreakupPriceD 'Safe

deriving instance FromJSON (EstimateBreakupPriceD 'Unsafe)

deriving instance ToJSON (EstimateBreakupPriceD 'Unsafe)
