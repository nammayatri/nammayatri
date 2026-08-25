{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.UI.Quote
  ( DQuote.GetQuotesRes (..),
    DQuote.OfferRes (..),
    DQuote.AlternateSuggestionsRes (..),
    API,
    getQuotes',
    handler,
  )
where

import qualified Domain.Action.UI.Quote as DQuote
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person as Person
import qualified Domain.Types.SearchRequest as SSR
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant hiding (throwError)
import Storage.Beam.SystemConfigs ()
import qualified Storage.Queries.SearchRequest as QSR
import Tools.Auth
import Tools.FlowHandling (withFlowHandlerAPIPersonId)

type API =
  QuotesAPI
    :<|> AlternateSuggestionAPI

type QuotesAPI =
  "rideSearch"
    :> Capture "searchId" (Id SSR.SearchRequest)
    :> TokenAuth
    :> "results"
    :> QueryParam "allowMultiple" Bool
    :> Get '[JSON] DQuote.GetQuotesRes

-- | Fares for the walk-and-save shapes other than the default.
--
-- Separate from /rideSearch/{id}/results because it answers on a different clock: the
-- default suggestion is priced before the search responds, while the alternates are
-- dispatched to the provider fire-and-forget and land whenever they land. Poll this until
-- 'allLoaded'; an alternate still outstanding is absent rather than empty.
type AlternateSuggestionAPI =
  "alternateSuggestion"
    :> Capture "searchId" (Id SSR.SearchRequest)
    :> TokenAuth
    :> "result"
    :> Get '[JSON] DQuote.AlternateSuggestionsRes

handler :: FlowServer API
handler =
  getQuotes
    :<|> getAlternateSuggestions

getAlternateSuggestions :: Id SSR.SearchRequest -> (Id Person.Person, Id Merchant.Merchant) -> FlowHandler DQuote.AlternateSuggestionsRes
getAlternateSuggestions searchRequestId (personId, _) = withFlowHandlerAPIPersonId personId . withPersonIdLogTag personId $ do
  searchRequest <- QSR.findById searchRequestId >>= fromMaybeM (SearchRequestDoesNotExist searchRequestId.getId)
  -- The suggestions belong to this customer's search; nobody else gets to read them.
  unless (searchRequest.riderId == personId) $ throwError AccessDenied
  DQuote.loadAlternateSuggestions searchRequest

getQuotes :: Id SSR.SearchRequest -> (Id Person.Person, Id Merchant.Merchant) -> Maybe Bool -> FlowHandler DQuote.GetQuotesRes
getQuotes searchRequestId (personId, merchantId) mbAllowMultiple = withFlowHandlerAPIPersonId personId . withPersonIdLogTag personId $ getQuotes' searchRequestId (personId, merchantId) mbAllowMultiple

getQuotes' :: Id SSR.SearchRequest -> (Id Person.Person, Id Merchant.Merchant) -> Maybe Bool -> Flow DQuote.GetQuotesRes
getQuotes' searchRequestId _ mbAllowMultiple = DQuote.getQuotes searchRequestId mbAllowMultiple
