{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.ProviderPlatform.Management.DriverGoHome
  ( getDriverGoHomeGetHomeLocation,
    postDriverGoHomeUpdateHomeLocation,
    postDriverGoHomeIncrementGoToCount,
    getDriverGoHomeGetGoHomeInfo,
  )
where

import qualified API.Client.ProviderPlatform.Management as Client
import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Management.DriverGoHome as Common
import qualified Dashboard.Common.Driver as Common
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import qualified "lib-dashboard" Domain.Types.Transaction as DT
import "lib-dashboard" Environment
import EulerHS.Prelude
import Kernel.Types.APISuccess (APISuccess (..))
import qualified Kernel.Types.Beckn.City as City
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.Transaction as T
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api
import Tools.Auth.Merchant

buildTransaction ::
  ( MonadFlow m,
    Common.HideSecrets request
  ) =>
  ApiTokenInfo ->
  Maybe (Id Common.Driver) ->
  Maybe request ->
  m DT.Transaction
buildTransaction apiTokenInfo driverId =
  T.buildTransaction (DT.castEndpoint apiTokenInfo.userActionType) (Just DRIVER_OFFER_BPP_MANAGEMENT) (Just apiTokenInfo) driverId Nothing

getDriverGoHomeGetHomeLocation :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Flow Common.GetHomeLocationsRes
getDriverGoHomeGetHomeLocation merchantShortId opCity apiTokenInfo driverId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callManagementAPI checkedMerchantId opCity (.driverGoHomeDSL.getDriverGoHomeGetHomeLocation) driverId

postDriverGoHomeUpdateHomeLocation :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Common.UpdateDriverHomeLocationReq -> Flow APISuccess
postDriverGoHomeUpdateHomeLocation merchantShortId opCity apiTokenInfo driverId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just driverId) $ Just req
  T.withTransactionStoring transaction $
    Client.callManagementAPI checkedMerchantId opCity (.driverGoHomeDSL.postDriverGoHomeUpdateHomeLocation) driverId req

postDriverGoHomeIncrementGoToCount :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Flow APISuccess
postDriverGoHomeIncrementGoToCount merchantShortId opCity apiTokenInfo driverId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just driverId) T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callManagementAPI checkedMerchantId opCity (.driverGoHomeDSL.postDriverGoHomeIncrementGoToCount) driverId

getDriverGoHomeGetGoHomeInfo :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Flow Common.CachedGoHomeRequestInfoRes
getDriverGoHomeGetGoHomeInfo merchantShortId opCity apiTokenInfo driverId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callManagementAPI checkedMerchantId opCity (.driverGoHomeDSL.getDriverGoHomeGetGoHomeInfo) driverId
