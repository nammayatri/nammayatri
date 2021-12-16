module API.Parking.Types where

import qualified API.Parking.Booking.Types as Booking
import qualified API.Parking.Search.Types as Search
import qualified API.Parking.SearchId.Quotes.Types as GetQuote
import API.Parking.Quotes.Types as ConfirmQuote
import Servant

type API =
  "parking"
    :> ( Search.API
           :<|> GetQuote.API
           :<|> Booking.API
           :<|> ConfirmQuote.API
       )
