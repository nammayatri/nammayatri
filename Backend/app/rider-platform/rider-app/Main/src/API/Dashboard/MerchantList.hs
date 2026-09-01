{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- Hand-written BAP-side merchant/operating-city list endpoints, mounted on rider-app's dashboard
-- (management) server. Deliberately NOT in the shared CommonAPIs spec so no rider-dashboard proxy
-- is generated. provider-dashboard (the single control center) calls these via a bespoke client
-- (RiderPlatformClient.RiderApp) and merges the result with the driver-app (BPP) list.
module API.Dashboard.MerchantList where

import qualified "dashboard-helper-api" Dashboard.Common.Merchant as Common
import qualified Domain.Action.Dashboard.Merchant as DMerchant
import qualified Domain.Types.Merchant as DM
import Environment
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant hiding (throwError)

type API =
  "merchant"
    :> "listWithCities"
    :> Get '[JSON] [Common.MerchantWithCities]

handler :: ShortId DM.Merchant -> Context.City -> FlowServer API
handler = getMerchantListWithCities

getMerchantListWithCities :: ShortId DM.Merchant -> Context.City -> FlowHandler [Common.MerchantWithCities]
getMerchantListWithCities merchantId city = withDashboardFlowHandlerAPI $ DMerchant.getMerchantListWithCities merchantId city
