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
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.SearchRequest as SSR
import Environment
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import SharedLogic.Merchant
import Storage.Beam.SystemConfigs ()

type API =
  "quote"
    :> CustomerGetQuoteAPI

type CustomerGetQuoteAPI =
  Capture "searchId" (Id SSR.SearchRequest)
    :> Capture "customerId" (Id DP.Person)
    :> "result"
    :> Get '[JSON] DQuote.GetQuotesRes

handler :: ShortId DM.Merchant -> FlowServer API
handler = callGetQuotes

callGetQuotes :: ShortId DM.Merchant -> Id SSR.SearchRequest -> Id DP.Person -> FlowHandler DQuote.GetQuotesRes
callGetQuotes merchantId req personId = do
  m <- withFlowHandlerAPI $ findMerchantByShortId merchantId
  UQ.getQuotes req (personId, m.id)
