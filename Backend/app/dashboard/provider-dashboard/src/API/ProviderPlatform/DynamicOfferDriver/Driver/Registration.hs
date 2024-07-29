{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.ProviderPlatform.DynamicOfferDriver.Driver.Registration
  ( API,
    handler,
  )
where

import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Management.DriverRegistration as Common
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import qualified "lib-dashboard" Domain.Types.Role as DRole
import "lib-dashboard" Environment
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Beckn.City as City
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified ProviderPlatformClient.DynamicOfferDriver.RideBooking as Client
import Servant
import Storage.Beam.CommonInstances ()
import "lib-dashboard" Storage.Queries.Person as QP
import "lib-dashboard" Storage.Queries.Role as QRole
import "lib-dashboard" Tools.Auth
import "lib-dashboard" Tools.Auth.Merchant
import "lib-dashboard" Tools.Error

type API =
  "driver"
    :> ( AuthAPI
           :<|> VerifyAPI
       )

handler :: ShortId DM.Merchant -> City.City -> FlowServer API
handler merchantId city =
  auth merchantId city
    :<|> verify merchantId city

type AuthAPI = ApiAuth 'DRIVER_OFFER_BPP 'DRIVERS 'AUTH :> Common.AuthAPI

type VerifyAPI = ApiAuth 'DRIVER_OFFER_BPP 'DRIVERS 'VERIFY :> Common.VerifyAPI

auth :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Common.AuthReq -> FlowHandler Common.AuthRes
auth merchantShortId opCity apiTokenInfo req =
  withFlowHandlerAPI' $ do
    checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
    Client.callDriverOfferBPP checkedMerchantId opCity (.driverRegistration.auth) req

verify :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Text -> Common.AuthVerifyReq -> FlowHandler APISuccess
verify merchantShortId opCity apiTokenInfo authId req =
  withFlowHandlerAPI' $ do
    checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
    encPerson <- QP.findById apiTokenInfo.personId >>= fromMaybeM (PersonNotFound apiTokenInfo.personId.getId)
    role <- QRole.findById encPerson.roleId >>= fromMaybeM (RoleNotFound encPerson.roleId.getId)
    let mbFleet = role.dashboardAccessType == DRole.FLEET_OWNER || role.dashboardAccessType == DRole.RENTAL_FLEET_OWNER
    Client.callDriverOfferBPP checkedMerchantId opCity (.driverRegistration.verify) authId mbFleet apiTokenInfo.personId.getId req
