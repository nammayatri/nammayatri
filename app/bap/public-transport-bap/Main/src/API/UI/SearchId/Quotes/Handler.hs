module API.UI.SearchId.Quotes.Handler where

import Beckn.Prelude
import Beckn.Types.Id
import Beckn.Utils.Common
import Domain.Action.UI.Quotes as DQuotes
import qualified Domain.Types.Search as DSearch
import Environment
import Tools.Auth

handler :: Id DSearch.Search -> PersonId -> FlowHandler DQuotes.GetQuotesRes
handler searchId _personId = withFlowHandlerAPI $ do
  DQuotes.getQuotesHandler searchId
