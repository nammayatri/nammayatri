module API.UI.SearchId.Quotes.Handler where

import API.UI.SearchId.Quotes.Types as Quotes
import App.Types
import Beckn.Prelude
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Domain.Types.Search as Domain
import Product.Quotes as Quotes
import Tools.Auth

handler :: Id Domain.Search -> PersonId -> FlowHandler Quotes.GetQuotesRes
handler searchId _personId = withFlowHandlerAPI $ do
  Quotes.getQuotesHandler searchId
