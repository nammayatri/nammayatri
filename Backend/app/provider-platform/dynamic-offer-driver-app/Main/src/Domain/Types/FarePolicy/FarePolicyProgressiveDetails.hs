{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.FarePolicy.FarePolicyProgressiveDetails
  ( module Reexport,
    module Domain.Types.FarePolicy.FarePolicyProgressiveDetails,
    WaitingChargeInfo (..),
    NightShiftCharge (..),
    WaitingCharge (..),
    PickupCharges (..),
    PickupChargesWithCurrency (..),
  )
where

import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Management.Merchant as Common
import Data.Aeson.Types
import Data.List.NonEmpty
import Domain.Types.Common
import Domain.Types.FarePolicy.Common
import Domain.Types.FarePolicy.FarePolicyProgressiveDetails.FarePolicyProgressiveDetailsPerExtraKmRateSection as Reexport
import Domain.Types.FarePolicy.FarePolicyProgressiveDetails.FarePolicyProgressiveDetailsPerMinRateSection as Reexport
import Kernel.Prelude as KP
import Kernel.Types.Common

data FPProgressiveDetailsD (s :: UsageSafety) = FPProgressiveDetails
  { baseFare :: HighPrecMoney,
    baseDistance :: Meters,
    distanceUnit :: DistanceUnit,
    perExtraKmRateSections :: NonEmpty (FPProgressiveDetailsPerExtraKmRateSectionD s),
    perMinRateSections :: Maybe (NonEmpty FPProgressiveDetailsPerMinRateSection),
    deadKmFare :: HighPrecMoney,
    pickupCharges :: PickupCharges,
    waitingChargeInfo :: Maybe WaitingChargeInfo,
    nightShiftCharge :: Maybe NightShiftCharge,
    currency :: Currency
  }
  deriving (Generic, Show, ToSchema)

type FPProgressiveDetails = FPProgressiveDetailsD 'Safe

instance FromJSON (FPProgressiveDetailsD 'Unsafe)

instance ToJSON (FPProgressiveDetailsD 'Unsafe)

-- FIXME remove
instance FromJSON (FPProgressiveDetailsD 'Safe)

-- FIXME remove
instance ToJSON (FPProgressiveDetailsD 'Safe)

-----------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------APIEntity--------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------

data FPProgressiveDetailsAPIEntity = FPProgressiveDetailsAPIEntity
  { baseFare :: Money,
    baseFareWithCurrency :: PriceAPIEntity,
    baseDistance :: Meters,
    perExtraKmRateSections :: NonEmpty FPProgressiveDetailsPerExtraKmRateSectionAPIEntity,
    perMinRateSections :: Maybe (NonEmpty FPProgressiveDetailsPerMinRateSectionAPIEntity),
    deadKmFare :: Money,
    pickupCharges :: PickupChargesApiEntity,
    pickupChargesWithCurrency :: PickupChargesWithCurrency,
    deadKmFareWithCurrency :: PriceAPIEntity,
    waitingChargeInfo :: Maybe Common.WaitingChargeInfoAPIEntity,
    nightShiftCharge :: Maybe Common.NightShiftChargeAPIEntity
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
