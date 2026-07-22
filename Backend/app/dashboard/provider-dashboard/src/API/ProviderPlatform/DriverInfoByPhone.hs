{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.ProviderPlatform.DriverInfoByPhone
  ( API,
    handler,
  )
where

import qualified "dynamic-offer-driver-app" API.Types.Dashboard.RideBooking
import qualified "dynamic-offer-driver-app" API.Types.Dashboard.RideBooking.Driver
import qualified Domain.Action.ProviderPlatform.RideBooking.DriverInfoByPhone as DIByPhone
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Types.Beckn.City as City
import Kernel.Types.Id
import Kernel.Utils.Common (withFlowHandlerAPI')
import Servant
import Tools.Auth.Api

-- | Reuses the GET_DRIVER_INFO user action type: this endpoint exposes exactly
-- the same data as /driver/info, just without requiring the caller to know the
-- driver's city, so it needs no separate access-matrix grant.
type API =
  "driver"
    :> "infoByPhone"
    :> ApiAuth
         ('DRIVER_OFFER_BPP)
         ('DSL)
         (('PROVIDER_RIDE_BOOKING) / ('API.Types.Dashboard.RideBooking.DRIVER) / ('API.Types.Dashboard.RideBooking.Driver.GET_DRIVER_INFO))
    :> QueryParam "mobileNumber" Text
    :> QueryParam "mobileCountryCode" Text
    :> Get '[JSON] API.Types.Dashboard.RideBooking.Driver.DriverInfoRes

handler :: ShortId DM.Merchant -> City.City -> FlowServer API
handler merchantId city apiTokenInfo mbMobileNumber mbMobileCountryCode =
  withFlowHandlerAPI' $
    DIByPhone.getDriverInfoByPhone merchantId city apiTokenInfo mbMobileNumber mbMobileCountryCode
