{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Domain.Types.FarePolicy.Common where

import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Management.Merchant as Common
import Data.Aeson
import EulerHS.Prelude hiding (length, map)
import Kernel.Beam.Lib.UtilsTH (mkBeamInstancesForJSON)
import Kernel.Prelude
import Kernel.Types.Common

data NightShiftCharge
  = ProgressiveNightShiftCharge Float
  | ConstantNightShiftCharge HighPrecMoney
  deriving stock (Show, Eq, Read, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data PickupCharges = PickupCharges
  { pickupChargesMin :: HighPrecMoney,
    pickupChargesMax :: HighPrecMoney
  }
  deriving stock (Show, Eq, Read, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

$(mkBeamInstancesForJSON ''PickupCharges)

data PickupChargesApiEntity = PickupChargesApiEntity
  { pickupChargesMin :: Money,
    pickupChargesMax :: Money
  }
  deriving stock (Show, Eq, Read, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data PickupChargesWithCurrency = PickupChargesWithCurrency
  { pickupChargesMinWithCurrency :: PriceAPIEntity,
    pickupChargesMaxWithCurrency :: PriceAPIEntity
  }
  deriving stock (Show, Eq, Read, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data WaitingChargeInfo = WaitingChargeInfo
  { freeWaitingTime :: Minutes,
    waitingCharge :: WaitingCharge
  }
  deriving stock (Show, Eq, Read, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data WaitingCharge
  = PerMinuteWaitingCharge HighPrecMoney
  | ConstantWaitingCharge HighPrecMoney
  deriving stock (Show, Eq, Read, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

$(mkBeamInstancesForJSON ''NightShiftCharge)
$(mkBeamInstancesForJSON ''WaitingCharge)

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
