{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.RiderPlatform
  ( API,
    APIV2,
    handler,
    handlerV2,
  )
where

import qualified API.Action.RiderPlatform.AppManagement as AppManagementDSL
import qualified API.Action.RiderPlatform.IssueManagement as IssueManagementDSL
import qualified API.Action.RiderPlatform.Management as ManagementDSL
import qualified API.Action.RiderPlatform.RideBooking as RideBookingDSL
import qualified "lib-dashboard" Domain.Types.Merchant as DMerchant
import "lib-dashboard" Environment
import qualified Kernel.Types.Beckn.City as City
import Kernel.Types.Id
import Servant

-- TODO: Deprecated, Remove after successful deployment
type API =
  "bap"
    :> Capture "merchantId" (ShortId DMerchant.Merchant)
    :> API'

type APIV2 =
  "bap"
    :> Capture "merchantId" (ShortId DMerchant.Merchant)
    :> Capture "city" City.City
    :> API'

type API' =
  ManagementDSL.API
    :<|> AppManagementDSL.API
    :<|> IssueManagementDSL.API
    :<|> ("rideBooking" :> RideBookingDSL.API)

-- TODO: Deprecated, Remove after successful deployment
handler :: FlowServer API
handler merchantId = do
  let city = getCity merchantId.getShortId
  ManagementDSL.handler merchantId city
    :<|> AppManagementDSL.handler merchantId city
    :<|> IssueManagementDSL.handler merchantId city
    :<|> RideBookingDSL.handler merchantId city
  where
    getCity = \case
      "NAMMA_YATRI" -> City.Bangalore
      "YATRI" -> City.Kochi
      "JATRI_SAATHI" -> City.Kolkata
      _ -> City.AnyCity

handlerV2 :: FlowServer APIV2
handlerV2 merchantId city =
  ManagementDSL.handler merchantId city
    :<|> AppManagementDSL.handler merchantId city
    :<|> IssueManagementDSL.handler merchantId city
    :<|> RideBookingDSL.handler merchantId city
