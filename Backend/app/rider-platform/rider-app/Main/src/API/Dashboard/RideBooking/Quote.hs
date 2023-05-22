{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}

module API.Dashboard.RideBooking.Quote where

import qualified API.UI.Quote as UQ
import qualified Domain.Action.UI.Quote as DQuote
import Environment
import Kernel.Types.Id
import Servant
import qualified SharedLogic.Types.Person as DP
import qualified SharedLogic.Types.SearchRequest as SSR

type API =
  "quote"
    :> CustomerGetQuoteAPI

type CustomerGetQuoteAPI =
  Capture "searchId" (Id SSR.SearchRequest)
    :> Capture "customerId" (Id DP.Person)
    :> "result"
    :> Get '[JSON] DQuote.GetQuotesRes

handler :: FlowServer API
handler = callGetQuotes

callGetQuotes :: Id SSR.SearchRequest -> Id DP.Person -> FlowHandler DQuote.GetQuotesRes
callGetQuotes = UQ.getQuotes
