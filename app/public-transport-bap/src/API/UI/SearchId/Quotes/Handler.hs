module API.UI.SearchId.Quotes.Handler where

import API.UI.SearchId.Quotes.Types as Quotes
import App.Types
import Beckn.Prelude
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Domain.Search as DSearch
import Product.Quotes as Quotes
import Tools.Auth

handler :: Id DSearch.Search -> PersonId -> FlowHandler Quotes.GetQuotesRes
handler searchId _personId = withFlowHandlerAPI $ do
  Quotes.getQuotesHandler searchId