module API.UI.Handler where

import qualified API.UI.Booking.Handler as Booking
import qualified API.UI.QuoteConfirm.Handler as QuoteConfirm
import API.UI.Search.Handler as Search
import qualified API.UI.SearchId.Quotes.Handler as Quotes
import qualified API.UI.Types as UI
import App.Types
import Servant

handler :: FlowServer UI.API
handler =
  Search.searchHandler
    :<|> Booking.handler
    :<|> Quotes.handler
    :<|> QuoteConfirm.handler
