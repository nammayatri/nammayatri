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
import Domain.Types.AccessMatrix.BAP
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import qualified "rider-app" Domain.Types.Person as DP
import qualified "rider-app" Domain.Types.SearchRequest as SSR
import "lib-dashboard" Environment
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified RiderPlatformClient.RiderApp as Client
import Servant
import "lib-dashboard" Tools.Auth
import Tools.Auth.Merchant

type API =
  "quote"
    :> ApiAuth ('AppBackendBAP ('CUSTOMERS 'GET_QUOTE))
    :> BAP.CustomerGetQuoteAPI

handler :: ShortId DM.Merchant -> FlowServer API
handler = callGetQuotes

callGetQuotes :: ShortId DM.Merchant -> ApiTokenInfo -> Id SSR.SearchRequest -> Id DP.Person -> FlowHandler DQuote.GetQuotesRes
callGetQuotes merchantShortId apiTokenInfo searchRequestId personId = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  Client.callRiderApp checkedMerchantId (.rideBooking.quote.getQuote) searchRequestId personId
