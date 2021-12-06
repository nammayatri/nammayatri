module API.Parking.Quotes.QuoteId.Types where

import API.Parking.Quotes.QuoteId.Confirm.Types as Confirm
import Beckn.Types.Id (Id)
import Domain.Quote (Quote)
import Servant

type API =
  Capture "quoteId" (Id Quote) :> Confirm.API
