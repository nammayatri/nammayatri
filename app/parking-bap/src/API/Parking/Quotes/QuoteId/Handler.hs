module API.Parking.Quotes.QuoteId.Handler where

import API.Parking.Quotes.QuoteId.Confirm.Handler as Confirm
import API.Parking.Quotes.QuoteId.Types
import App.Types

handler :: FlowServer API
handler = Confirm.handler
