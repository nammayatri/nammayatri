{-# LANGUAGE TypeApplications #-}

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

data OneWayQuote = OneWayQuote
  { id :: Id Quote,
    requestId :: Id DSR.SearchRequest,
    productId :: Id Products, -- do we need this field?
    estimatedFare :: Amount,
    discount :: Maybe Amount,
    estimatedTotalFare :: Amount,
    providerId :: Id DOrg.Organization,
    vehicleVariant :: DVeh.Variant,
    createdAt :: UTCTime,
    distance :: Double,
    distanceToNearestDriver :: Double
  }

data RentalQuote = RentalQuote
  { id :: Id Quote,
    requestId :: Id DSR.SearchRequest,
    productId :: Id Products, -- do we need this field?
    estimatedFare :: Amount,
    discount :: Maybe Amount,
    estimatedTotalFare :: Amount,
    providerId :: Id DOrg.Organization,
    vehicleVariant :: DVeh.Variant,
    createdAt :: UTCTime
  }

data Quote = OneWay OneWayQuote | Rental RentalQuote

getDistance :: Quote -> Maybe Double
getDistance (OneWay quote) = Just quote.distance
getDistance (Rental _) = Nothing

instance (HasField x OneWayQuote a, HasField x RentalQuote a) => HasField x Quote a where
  hasField (OneWay quote) = do
    let setter newField = OneWay $ fst (hasField @x quote) newField
    (setter, snd (hasField @x quote))
  hasField (Rental quote) = do
    let setter newField = Rental $ fst (hasField @x quote) newField
    (setter, snd (hasField @x quote))
