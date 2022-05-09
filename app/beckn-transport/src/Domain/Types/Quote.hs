module Domain.Types.Quote where

import Beckn.Types.Amount
import Beckn.Types.Id
import Data.Time
import qualified Domain.Types.Organization as DOrg
import Domain.Types.Products (Products)
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.Vehicle as DVeh
import EulerHS.Prelude hiding (id)
import GHC.Records.Extra

data Quote = Quote
  { id :: Id Quote,
    requestId :: Id DSR.SearchRequest,
    productId :: Id Products, -- do we need this field?
    estimatedFare :: Amount,
    discount :: Maybe Amount,
    estimatedTotalFare :: Amount,
    providerId :: Id DOrg.Organization,
    vehicleVariant :: DVeh.Variant,
    createdAt :: UTCTime,
    quoteDetails :: QuoteDetails
  }

data QuoteDetails = OneWayDetails OneWayQuoteDetails | RentalDetails

data OneWayQuoteDetails = OneWayQuoteDetails
  { distance :: Double,
    distanceToNearestDriver :: Double
  }

getDistance :: QuoteDetails -> Maybe Double
getDistance quoteDetails = case quoteDetails of
  RentalDetails -> Nothing
  OneWayDetails oneWayDetails -> Just oneWayDetails.distance
