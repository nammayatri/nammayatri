module Domain.Types.Quote.QuoteTerms where

import Beckn.Prelude
import Beckn.Types.Id
import qualified Domain.Types.Quote as DQuote

data QuoteTermsEntity = QuoteTermsEntity
  { id :: Id QuoteTermsEntity,
    quoteId :: Id DQuote.Quote,
    description :: Text
  }
  deriving (Generic, Show)

mkQuoteTerms :: QuoteTermsEntity -> DQuote.QuoteTerms
mkQuoteTerms QuoteTermsEntity {..} =
  DQuote.QuoteTerms
    { id = cast id,
      ..
    }

mkQuoteTermsEntities :: DQuote.Quote -> [QuoteTermsEntity]
mkQuoteTermsEntities DQuote.Quote {..} =
  case quoteDetails of
    DQuote.OneWayDetails _ -> []
    DQuote.RentalDetails rentalDetails -> mkQuoteTermsEntity id <$> rentalDetails.quoteTerms

mkQuoteTermsEntity :: Id DQuote.Quote -> DQuote.QuoteTerms -> QuoteTermsEntity
mkQuoteTermsEntity quoteId DQuote.QuoteTerms {..} =
  QuoteTermsEntity
    { id = cast id,
      ..
    }
