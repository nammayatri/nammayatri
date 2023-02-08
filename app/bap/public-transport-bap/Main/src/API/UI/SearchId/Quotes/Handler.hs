module API.UI.SearchId.Quotes.Handler where

import Domain.Action.UI.Quotes as DQuotes
import qualified Domain.Types.Search as DSearch
import Environment
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Tools.Auth

handler :: Id DSearch.Search -> PersonId -> FlowHandler DQuotes.GetQuotesRes
handler searchId _personId = withFlowHandlerAPI $ do
  DQuotes.getQuotesHandler searchId
