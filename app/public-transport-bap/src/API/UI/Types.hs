module API.UI.Types where

import qualified API.UI.Booking.Types as Booking
import qualified API.UI.QuoteConfirm.Types as QuoteConfirm
import API.UI.SearchId.Quotes.Types as Quotes
import Servant

type API =
  "ui"
    :> ( Booking.API
           :<|> Quotes.API
           :<|> QuoteConfirm.API
       )
