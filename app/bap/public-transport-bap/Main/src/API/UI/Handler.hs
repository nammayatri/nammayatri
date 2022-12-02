module API.UI.Handler where

import qualified API.UI.Booking.Handler as Booking
import qualified API.UI.QuoteConfirm.Handler as QuoteConfirm
import qualified API.UI.SearchId.Quotes.Handler as Quotes
import qualified API.UI.Types as UI
import Environment
import Servant

handler :: FlowServer UI.API
handler =
  Booking.handler
    :<|> Quotes.handler
    :<|> QuoteConfirm.handler
