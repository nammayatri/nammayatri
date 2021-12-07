module API.Parking.SearchId.Quotes.Handler where

import qualified API.Parking.SearchId.Quotes.Types as Quotes
import App.Types
import Beckn.Prelude
import Beckn.Types.Id
import Beckn.Utils.Error (withFlowHandlerAPI)
import qualified Domain.Search as DSearch
import qualified Storage.Queries.Quote as QQuote
import Tools.Auth (PersonId)

-- TODO Do we need to check that personId == search.requestorId?
handler :: Id DSearch.Search -> PersonId -> FlowHandler Quotes.GetQuotesRes
handler searchId _personId = withFlowHandlerAPI $ do
  QQuote.findAllBySearchId searchId
    <&> Quotes.GetQuotesRes
