{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.RiderPlatform.HotSpot
  ( API,
    handler,
  )
where

import qualified "rider-app" API.Dashboard.HotSpot as ADH
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import "lib-dashboard" Environment
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess (..))
import qualified Kernel.Types.Beckn.City as City
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified RiderPlatformClient.RiderApp.Operations as Client
import Servant hiding (throwError)
import "lib-dashboard" Tools.Auth
import "lib-dashboard" Tools.Auth.Merchant

type API = "hotSpot" :> ExpiredHotSpotAPI

type ExpiredHotSpotAPI =
  ApiAuth 'APP_BACKEND_MANAGEMENT 'CUSTOMERS 'REMOVE_EXPIRED_HOTSPOTS
    :> ADH.RemoveExpiredHotSpot

handler :: ShortId DM.Merchant -> City.City -> FlowServer API
handler = removeExpires

removeExpires :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> FlowHandler APISuccess
removeExpires merchantShortId opCity apiTokenInfo = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callRiderAppOperations checkedMerchantId opCity (.hotSpot.removeExpires)
