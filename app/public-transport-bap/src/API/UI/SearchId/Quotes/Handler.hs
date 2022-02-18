module API.UI.SearchId.Quotes.Handler where

import App.Types
import Beckn.Prelude
import Beckn.Types.Id
import Beckn.Utils.Common
import Domain.Endpoints.UI.Quotes as DQuotes
import qualified Domain.Types.Search as DSearch
import Tools.Auth

handler :: Id DSearch.Search -> PersonId -> FlowHandler DQuotes.GetQuotesRes
handler searchId _personId = withFlowHandlerAPI $ do
  DQuotes.getQuotesHandler searchId
