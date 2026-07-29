{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | @GET \/bpp\/driver-offer\/{merchantId}\/driver\/infoByPhoneNumber@
--
-- A city-less sibling of @\/driver\/info@, for callers that hold a phone number but do not
-- know which city the driver belongs to. Hand-written rather than DSL-generated because the
-- RideBooking dsl-config emits the servant API and domain handler into
-- @dynamic-offer-driver-app@, and this endpoint is deliberately dashboard-only.
module API.ProviderPlatform.DriverInfoByPhoneNumber
  ( API,
    handler,
  )
where

import qualified "dynamic-offer-driver-app" API.Types.Dashboard.RideBooking
import qualified "dynamic-offer-driver-app" API.Types.Dashboard.RideBooking.Driver
import qualified Domain.Action.ProviderPlatform.RideBooking.DriverInfoByPhoneNumber as Domain
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import "lib-dashboard" Environment
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (withFlowHandlerAPI')
import Servant
import Tools.Auth.Api

-- Reuses the GET_DRIVER_INFO action type on purpose: this endpoint exposes a strict subset
-- of what the caller could already obtain by logging into each of their granted cities in
-- turn, so it needs no separate access_matrix grant.
type API =
  "driver"
    :> "infoByPhoneNumber"
    :> ApiAuth
         'DRIVER_OFFER_BPP
         'DSL
         ('PROVIDER_RIDE_BOOKING / 'API.Types.Dashboard.RideBooking.DRIVER / 'API.Types.Dashboard.RideBooking.Driver.GET_DRIVER_INFO)
    :> QueryParam "mobileNumber" Text
    :> QueryParam "mobileCountryCode" Text
    :> Get '[JSON] API.Types.Dashboard.RideBooking.Driver.DriverInfoRes

handler :: ShortId DM.Merchant -> FlowServer API
handler merchantId apiTokenInfo mbMobileNumber mbMobileCountryCode =
  withFlowHandlerAPI' $
    Domain.getDriverInfoByPhoneNumber merchantId apiTokenInfo mbMobileNumber mbMobileCountryCode
