module Domain.Types.OneWayQuote where

import Beckn.Types.Id
import qualified Domain.Types.Quote as DQuote
import EulerHS.Prelude hiding (id)

data OneWayQuote = OneWayQuote
  { id :: Id OneWayQuote,
    quoteId :: Id DQuote.Quote,
    distance :: Double,
    distanceToNearestDriver :: Double
  }
  deriving (Generic, Show, Eq)
