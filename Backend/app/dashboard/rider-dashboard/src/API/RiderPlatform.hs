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

import qualified API.Action.RiderPlatform.Management as ManagementDSL
-- import qualified API.Action.RiderPlatform.RideBooking as RideBookingDSL
import qualified API.RiderPlatform.HotSpot as HotSpot
import qualified API.RiderPlatform.Issue as Issue
import qualified API.RiderPlatform.IssueList as IssueList
import qualified API.RiderPlatform.RideBooking as RideBooking
import qualified API.RiderPlatform.Tickets as Tickets
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
  RideBooking.API
    :<|> IssueList.API
    :<|> Issue.API
    :<|> Tickets.API
    :<|> HotSpot.API
    :<|> ManagementDSL.API

-- :<|> RideBookingDSL.API

-- TODO: Deprecated, Remove after successful deployment
handler :: FlowServer API
handler merchantId = do
  let city = getCity merchantId.getShortId
  RideBooking.handler merchantId city
    :<|> IssueList.handler merchantId city
    :<|> Issue.handler merchantId city
    :<|> Tickets.handler merchantId city
    :<|> HotSpot.handler merchantId city
    :<|> ManagementDSL.handler merchantId city
  where
    -- :<|> RideBookingDSL.handler merchantId city

    getCity = \case
      "NAMMA_YATRI" -> City.Bangalore
      "YATRI" -> City.Kochi
      "JATRI_SAATHI" -> City.Kolkata
      _ -> City.AnyCity

handlerV2 :: FlowServer APIV2
handlerV2 merchantId city =
  RideBooking.handler merchantId city
    :<|> IssueList.handler merchantId city
    :<|> Issue.handler merchantId city
    :<|> Tickets.handler merchantId city
    :<|> HotSpot.handler merchantId city
    :<|> ManagementDSL.handler merchantId city

-- :<|> RideBokingDSL.handler merchantId city
