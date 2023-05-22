{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Types.MerchantConfig where

import Kernel.Prelude
import Kernel.Types.Id
import qualified Kernel.Types.SlidingWindowCounters as SWC
import SharedLogic.Types.Merchant (Merchant)

-- Non empty list here?
data MerchantConfig = MerchantConfig
  { id :: Id MerchantConfig,
    merchantId :: Id Merchant,
    simulatedBookingCancellationCountThreshold :: Int,
    simulatedBookingCancellationCountWindow :: SWC.SlidingWindowOptions,
    simulatedBookingTotalCountThreshold :: Int,
    simulatedBookingCancelledByDriverCountThreshold :: Int,
    simulatedBookingCancelledByDriverCountWindow :: SWC.SlidingWindowOptions,
    simulatedSearchCountThreshold :: Int,
    simulatedSearchCountWindow :: SWC.SlidingWindowOptions,
    shouldSimulate :: Bool,
    enabled :: Bool
  }
  deriving (Generic, Show, ToJSON, FromJSON)
