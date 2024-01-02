{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.ProviderPlatform.DynamicOfferDriver.Driver.Coin
  ( API,
    handler,
  )
where

import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Driver.Coin as Common
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import "lib-dashboard" Environment
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Beckn.City as City
import Kernel.Types.Id
import Kernel.Utils.Common (withFlowHandlerAPI')
import qualified ProviderPlatformClient.DynamicOfferDriver.Operations as Client
import Servant
import "lib-dashboard" Tools.Auth
import "lib-dashboard" Tools.Auth.Merchant

type API =
  "coins"
    :> DriverBulkUploadCoinsAPI

handler :: ShortId DM.Merchant -> City.City -> FlowServer API
handler = driverBulkUploadCoins

type DriverBulkUploadCoinsAPI = ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'DRIVERS 'DRIVER_COIN_BULK_UPLOAD :> Common.BulkUploadCoinsAPI

driverBulkUploadCoins :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Common.BulkUploadCoinsReq -> FlowHandler APISuccess
driverBulkUploadCoins merchantShortId opCity apiTokenInfo Common.BulkUploadCoinsReq {..} = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callDriverOfferBPPOperations checkedMerchantId opCity (.drivers.driverCoins.driverCoinBulkUpload) Common.BulkUploadCoinsReq {..}
