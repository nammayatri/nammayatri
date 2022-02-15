module API.Parking.Handler where

import qualified API.Parking.Booking.Handler as Booking
import API.Parking.Quotes.Handler as ConfirmQuote
import qualified API.Parking.Search.Handler as Search
import qualified API.Parking.SearchId.Quotes.Handler as GetQuote
import qualified API.Parking.Types as Parking
import App.Types
import Servant

handler :: FlowServer Parking.API
handler =
  Search.handler
    :<|> GetQuote.handler
    :<|> Booking.handler
    :<|> ConfirmQuote.handler
