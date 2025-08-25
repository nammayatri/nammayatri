{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.ProviderPlatform.Management.DriverCoins
  ( postDriverCoinsBulkUploadCoins,
    postDriverCoinsBulkUploadCoinsV2,
    getDriverCoinsCoinHistory,
    postDriverCoinsBlacklistedEventsUpdate,
  )
where

import qualified API.Client.ProviderPlatform.Management as Client
import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Management.DriverCoins as Common
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import "lib-dashboard" Environment
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess (..))
import Kernel.Types.Beckn.City as City
import Kernel.Types.Id
import "lib-dashboard" Tools.Auth
import "lib-dashboard" Tools.Auth.Merchant

postDriverCoinsBulkUploadCoins :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Common.BulkUploadCoinsReq -> Flow Common.BulkUploadCoinRes
postDriverCoinsBulkUploadCoins merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callManagementAPI checkedMerchantId opCity (.driverCoinsDSL.postDriverCoinsBulkUploadCoins) req

postDriverCoinsBulkUploadCoinsV2 :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Common.BulkUploadCoinsReqV2 -> Flow Common.BulkUploadCoinRes
postDriverCoinsBulkUploadCoinsV2 merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callManagementAPI checkedMerchantId opCity (.driverCoinsDSL.postDriverCoinsBulkUploadCoinsV2) req

getDriverCoinsCoinHistory :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Maybe Integer -> Maybe Integer -> Flow Common.CoinHistoryRes
getDriverCoinsCoinHistory merchantShortId opCity apiTokenInfo driverId mbLimit mbOffset = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callManagementAPI checkedMerchantId opCity (.driverCoinsDSL.getDriverCoinsCoinHistory) driverId mbLimit mbOffset

postDriverCoinsBlacklistedEventsUpdate :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Common.UpdateBlacklistedCoinEventsReq -> Flow APISuccess
postDriverCoinsBlacklistedEventsUpdate merchantShortId opCity apiTokenInfo driverId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callManagementAPI checkedMerchantId opCity (.driverCoinsDSL.postDriverCoinsBlacklistedEventsUpdate) driverId req
