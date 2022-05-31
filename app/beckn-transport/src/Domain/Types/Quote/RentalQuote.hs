module Domain.Types.Quote.RentalQuote where

import Beckn.Types.Id
import qualified Domain.Types.Quote as DQuote
import qualified Domain.Types.RentalFarePolicy as DRentalFP
import EulerHS.Prelude hiding (id)

-- Not used in business logic, only for Tabular
data RentalQuote = RentalQuote
  { quoteId :: Id DQuote.Quote,
    rentalFarePolicyId :: Id DRentalFP.RentalFarePolicy
  }
  deriving (Generic, Show, Eq)

mkRentalQuote :: Id DQuote.Quote -> DQuote.RentalQuoteDetails -> RentalQuote
mkRentalQuote quoteId DQuote.RentalQuoteDetails {..} = RentalQuote {..}
