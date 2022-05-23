module Domain.Types.Quote.OneWayQuote where

import Beckn.Types.Id
import qualified Domain.Types.Quote as DQuote
import EulerHS.Prelude hiding (id)

-- Not used in business logic, only for Tabular
data OneWayQuote = OneWayQuote
  { quoteId :: Id DQuote.Quote,
    distance :: Double,
    distanceToNearestDriver :: Double
  }
  deriving (Generic, Show, Eq)

mkOneWayQuote :: Id DQuote.Quote -> DQuote.OneWayQuoteDetails -> OneWayQuote
mkOneWayQuote quoteId DQuote.OneWayQuoteDetails {..} = OneWayQuote {..}
