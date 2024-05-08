{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Domain.Types.FarePolicy.FarePolicyProgressiveDetails
  ( module Reexport,
    module Domain.Types.FarePolicy.FarePolicyProgressiveDetails,
  )
where

import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Merchant as Common
import Data.Aeson.Types
import Data.List.NonEmpty
import Domain.Types.Common
import Domain.Types.FarePolicy.FarePolicyProgressiveDetails.FarePolicyProgressiveDetailsPerExtraKmRateSection as Reexport
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude as KP
import Kernel.Types.Common

data FPProgressiveDetailsD (s :: UsageSafety) = FPProgressiveDetails
  { baseFare :: HighPrecMoney,
    baseDistance :: Meters,
    perExtraKmRateSections :: NonEmpty (FPProgressiveDetailsPerExtraKmRateSectionD s),
    deadKmFare :: HighPrecMoney,
    waitingChargeInfo :: Maybe WaitingChargeInfo,
    nightShiftCharge :: Maybe NightShiftCharge,
    currency :: Currency
  }
  deriving (Generic, Show)

data NightShiftCharge
  = ProgressiveNightShiftCharge Float
  | ConstantNightShiftCharge HighPrecMoney
  deriving stock (Show, Eq, Read, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

data WaitingChargeInfo = WaitingChargeInfo
  { freeWaitingTime :: Minutes,
    waitingCharge :: WaitingCharge
  }
  deriving stock (Show, Eq, Read, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

data WaitingCharge
  = PerMinuteWaitingCharge HighPrecMoney
  | ConstantWaitingCharge HighPrecMoney
  deriving stock (Show, Eq, Read, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

type FPProgressiveDetails = FPProgressiveDetailsD 'Safe

instance FromJSON (FPProgressiveDetailsD 'Unsafe)

instance ToJSON (FPProgressiveDetailsD 'Unsafe)

-- FIXME remove
instance FromJSON (FPProgressiveDetailsD 'Safe)

-- FIXME remove
instance ToJSON (FPProgressiveDetailsD 'Safe)

$(mkBeamInstancesForJSON ''NightShiftCharge)
$(mkBeamInstancesForJSON ''WaitingCharge)

-----------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------APIEntity--------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------

data FPProgressiveDetailsAPIEntity = FPProgressiveDetailsAPIEntity
  { baseFare :: Money,
    baseFareWithCurrency :: PriceAPIEntity,
    baseDistance :: Meters,
    perExtraKmRateSections :: NonEmpty FPProgressiveDetailsPerExtraKmRateSectionAPIEntity,
    deadKmFare :: Money,
    deadKmFareWithCurrency :: PriceAPIEntity,
    waitingChargeInfo :: Maybe Common.WaitingChargeInfoAPIEntity,
    nightShiftCharge :: Maybe Common.NightShiftChargeAPIEntity
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

mkWaitingChargeInfo :: Common.WaitingChargeInfoAPIEntity -> WaitingChargeInfo
mkWaitingChargeInfo Common.WaitingChargeInfoAPIEntity {..} =
  WaitingChargeInfo
    { waitingCharge = mkWaitingCharge waitingCharge,
      ..
    }

getWaitingChargeInfoFields :: Common.WaitingChargeInfoAPIEntity -> [Maybe PriceAPIEntity]
getWaitingChargeInfoFields Common.WaitingChargeInfoAPIEntity {..} = getWaitingChargeFields waitingCharge

mkWaitingCharge :: Common.WaitingChargeAPIEntity -> WaitingCharge
mkWaitingCharge (Common.PerMinuteWaitingCharge charge) = PerMinuteWaitingCharge charge
mkWaitingCharge (Common.ConstantWaitingCharge charge) = ConstantWaitingCharge $ toHighPrecMoney charge
mkWaitingCharge (Common.PerMinuteWaitingChargeWithCurrency charge) = PerMinuteWaitingCharge charge.amount
mkWaitingCharge (Common.ConstantWaitingChargeWithCurrency charge) = ConstantWaitingCharge $ toHighPrecMoney charge.amount

getWaitingChargeFields :: Common.WaitingChargeAPIEntity -> [Maybe PriceAPIEntity]
getWaitingChargeFields (Common.PerMinuteWaitingCharge _) = []
getWaitingChargeFields (Common.ConstantWaitingCharge _) = []
getWaitingChargeFields (Common.PerMinuteWaitingChargeWithCurrency charge) = [Just charge]
getWaitingChargeFields (Common.ConstantWaitingChargeWithCurrency charge) = [Just charge]

mkNightShiftCharge :: Common.NightShiftChargeAPIEntity -> NightShiftCharge
mkNightShiftCharge (Common.ProgressiveNightShiftCharge charge) = ProgressiveNightShiftCharge charge
mkNightShiftCharge (Common.ConstantNightShiftCharge charge) = ConstantNightShiftCharge $ toHighPrecMoney charge
mkNightShiftCharge (Common.ConstantNightShiftChargeWithCurrency charge) = ConstantNightShiftCharge charge.amount

getNightShiftChargeFields :: Common.NightShiftChargeAPIEntity -> [Maybe PriceAPIEntity]
getNightShiftChargeFields (Common.ProgressiveNightShiftCharge _) = []
getNightShiftChargeFields (Common.ConstantNightShiftCharge _) = []
getNightShiftChargeFields (Common.ConstantNightShiftChargeWithCurrency charge) = [Just charge]
