module Domain.Types.Quote where

import Beckn.Types.Amount
import Beckn.Types.Common
import Beckn.Types.Id
import Data.Time
import qualified Domain.Types.FarePolicy.FareProduct as DFareProduct
import qualified Domain.Types.FarePolicy.RentalFarePolicy as DRentalFP
import qualified Domain.Types.Organization as DOrg
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.Vehicle as DVeh
import EulerHS.Prelude hiding (id)
import GHC.Records.Extra

data Quote = Quote
  { id :: Id Quote,
    requestId :: Id DSR.SearchRequest,
    estimatedFare :: Amount,
    discount :: Maybe Amount,
    estimatedTotalFare :: Amount,
    providerId :: Id DOrg.Organization,
    vehicleVariant :: DVeh.Variant,
    createdAt :: UTCTime,
    quoteDetails :: QuoteDetails
  }

data QuoteDetails = OneWayDetails OneWayQuoteDetails | RentalDetails DRentalFP.RentalFarePolicy

data OneWayQuoteDetails = OneWayQuoteDetails
  { distance :: HighPrecMeters,
    distanceToNearestDriver :: HighPrecMeters
  }

getFareProductType :: QuoteDetails -> DFareProduct.FareProductType
getFareProductType = \case
  OneWayDetails _ -> DFareProduct.ONE_WAY
  RentalDetails _ -> DFareProduct.RENTAL
