{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.MerchantConfig where

import Domain.Types.Merchant (Merchant)
import Kernel.Prelude
import Kernel.Types.Id
import qualified Kernel.Types.SlidingWindowCounters as SWC

-- Non empty list here?
data MerchantConfig = MerchantConfig
  { merchantId :: Id Merchant,
    fraudBookingDetectionWindow :: SWC.SlidingWindowOptions,
    fraudBookingCancellationCountThreshold :: Int,
    fraudBookingTotalCountThreshold :: Int
  }
  deriving (Generic, Show, ToJSON, FromJSON)
