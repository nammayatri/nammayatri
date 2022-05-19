module Domain.Types.Quote.RentalQuote where

import Beckn.Prelude
import Beckn.Types.Common
import Beckn.Types.Id
import qualified Domain.Types.Quote as DQuote

-- Not used in business logic, only for Tabular
data RentalQuote = RentalQuote
  { quoteId :: Id DQuote.Quote,
    baseDistance :: Kilometers,
    baseDuration :: Hours
  }
  deriving (Generic, Show)

mkRentalQuote :: Id DQuote.Quote -> DQuote.RentalQuoteDetails -> RentalQuote
mkRentalQuote quoteId DQuote.RentalQuoteDetails {..} = RentalQuote {..}
