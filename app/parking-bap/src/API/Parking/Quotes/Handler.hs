module API.Parking.Quotes.Handler where

import API.Parking.Quotes.QuoteId.Handler as QuoteId
import API.Parking.Quotes.Types
import App.Types

handler :: FlowServer API
handler = QuoteId.handler
