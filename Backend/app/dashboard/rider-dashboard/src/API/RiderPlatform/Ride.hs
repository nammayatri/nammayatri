{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.RiderPlatform.Ride
  ( API,
    handler,
  )
where

import qualified Dashboard.RiderPlatform.Ride as Common
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import "lib-dashboard" Environment
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.SlidingWindowLimiter
import qualified RiderPlatformClient.RiderApp as Client
import Servant hiding (throwError)
import Tools.Auth.Merchant

type API =
  "ride"
    :> ShareRideInfoAPI

type ShareRideInfoAPI = Common.ShareRideInfoAPI

handler :: ShortId DM.Merchant -> FlowServer API
handler = shareRideInfo

rideInfoHitsCountKey :: Id Common.Ride -> Text
rideInfoHitsCountKey rideId = "RideInfoHits:" <> getId rideId <> ":hitsCount"

shareRideInfo ::
  ShortId DM.Merchant ->
  Id Common.Ride ->
  FlowHandler Common.ShareRideInfoRes
shareRideInfo merchantShortId rideId = withFlowHandlerAPI $ do
  checkSlidingWindowLimit (rideInfoHitsCountKey rideId)
  checkedMerchantId <- merchantAccessCheck merchantShortId merchantShortId
  Client.callRiderApp checkedMerchantId (.rides.shareRideInfo) rideId
