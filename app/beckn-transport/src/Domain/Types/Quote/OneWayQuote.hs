module Domain.Types.Quote.OneWayQuote where

import Beckn.Types.Id
import qualified Domain.Types.Quote as DQuote
import EulerHS.Prelude hiding (id)

-- Not used in business logic, only for Tabular
data OneWayQuoteEntity = OneWayQuoteEntity
  { quoteId :: Id DQuote.Quote,
    distance :: Double,
    distanceToNearestDriver :: Double
  }
  deriving (Generic, Show, Eq)

mkOneWayQuoteEntity :: DQuote.OneWayQuote -> OneWayQuoteEntity
mkOneWayQuoteEntity DQuote.OneWayQuote {..} =
  OneWayQuoteEntity
    { quoteId = id,
      ..
    }
