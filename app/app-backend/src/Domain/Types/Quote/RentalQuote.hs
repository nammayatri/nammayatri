module Domain.Types.Quote.RentalQuote where

import Beckn.Prelude
import Beckn.Types.Id
import qualified Domain.Types.Quote as DQuote

-- Not used in business logic, only for Tabular
data RentalQuote = RentalQuote
  { quoteId :: Id DQuote.Quote,
    baseDistance :: Double,
    baseDurationHr :: Int
  }
  deriving (Generic, Show)

mkRentalQuote :: Id DQuote.Quote -> DQuote.RentalQuoteDetails -> RentalQuote
mkRentalQuote quoteId DQuote.RentalQuoteDetails {..} = RentalQuote {..}
