{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.SDKEvents where

import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import Environment
import Kernel.Prelude
import qualified Kernel.Types.APISuccess as APISuccess
import Kernel.Types.Id
import Tools.Event

data SDKEventsReq = SDKEventsReq
  { event :: Text
  }

sdkEvents :: (Id DP.Person, Id Merchant.Merchant, Id DMOC.MerchantOperatingCity) -> SDKEventsReq -> Flow APISuccess.APISuccess
sdkEvents (personId, merchantId, merchantOperatingCityId) SDKEventsReq {..} = do
  triggerSDKEvent $ SDKEventData personId merchantId merchantOperatingCityId event
  return APISuccess.Success
