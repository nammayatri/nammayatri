module Domain.Types.Quote where

import Beckn.Types.Amount
import Beckn.Types.Id
import Data.Time
import qualified Domain.Types.FareProduct as DFareProduct
import qualified Domain.Types.Organization as DOrg
import Domain.Types.Products (Products)
import qualified Domain.Types.RentalFarePolicy as DRentalFarePolicy
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

data QuoteDetails = OneWayDetails OneWayQuoteDetails | RentalDetails RentalQuoteDetails

data OneWayQuoteDetails = OneWayQuoteDetails
  { distance :: Double,
    distanceToNearestDriver :: Double
  }

data RentalQuoteDetails = RentalQuoteDetails
  { baseDistance :: Double,
    baseDurationHr :: Int,
    descriptions :: [Text]
  }

getFareProductType :: QuoteDetails -> DFareProduct.FareProductType
getFareProductType = \case
  OneWayDetails _ -> DFareProduct.ONE_WAY
  RentalDetails _ -> DFareProduct.RENTAL

mkRentalQuoteDetails :: DRentalFarePolicy.RentalFarePolicy -> QuoteDetails
mkRentalQuoteDetails rentalFarePolicy@DRentalFarePolicy.RentalFarePolicy {..} =
  RentalDetails $
    RentalQuoteDetails
      { descriptions = mkDescriptions rentalFarePolicy,
        ..
      }

mkDescriptions :: DRentalFarePolicy.RentalFarePolicy -> [Text]
mkDescriptions DRentalFarePolicy.RentalFarePolicy {..} =
  [ "Extra km fare: " <> show extraKmFare,
    "Extra min fare: " <> show extraMinuteFare,
    "Extra fare for day: " <> maybe "not allowed" show driverAllowanceForDay,
    "A rider can choose this package for a trip where the rider may not have a pre-decided destination and may not want to return to the origin location",
    "The rider may want to stop at multiple destinations and have the taxi wait for the rider at these locations"
  ]
