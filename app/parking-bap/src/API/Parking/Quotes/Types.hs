module API.Parking.Quotes.Types where

import API.Parking.Quotes.QuoteId.Types as QuoteId
import Servant

type API = "quotes" :> QuoteId.API
