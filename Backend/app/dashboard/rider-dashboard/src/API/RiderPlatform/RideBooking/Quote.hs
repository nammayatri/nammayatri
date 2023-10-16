{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.RiderPlatform.RideBooking.Quote where

import qualified "rider-app" API.Dashboard.RideBooking.Quote as BAP
import qualified "rider-app" Domain.Action.UI.Quote as DQuote
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import qualified "rider-app" Domain.Types.Person as DP
import qualified "rider-app" Domain.Types.SearchRequest as SSR
import "lib-dashboard" Environment
import Kernel.Prelude
import qualified Kernel.Types.Beckn.City as City
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified RiderPlatformClient.RiderApp as Client
import Servant
import "lib-dashboard" Tools.Auth
import Tools.Auth.Merchant

type API =
  "quote"
    :> ApiAuth 'APP_BACKEND 'CUSTOMERS 'GETQUOTE
    :> BAP.CustomerGetQuoteAPI

handler :: ShortId DM.Merchant -> City.City -> FlowServer API
handler = callGetQuotes

callGetQuotes :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id SSR.SearchRequest -> Id DP.Person -> FlowHandler DQuote.GetQuotesRes
callGetQuotes merchantShortId opCity apiTokenInfo searchRequestId personId = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callRiderApp checkedMerchantId (.rideBooking.quote.getQuote) searchRequestId personId
