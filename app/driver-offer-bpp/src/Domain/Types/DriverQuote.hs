module Domain.Types.DriverQuote where

import Beckn.Prelude
import Beckn.Types.Common
import Beckn.Types.Id
import Domain.Types.Person
import Domain.Types.SearchRequest
import qualified Domain.Types.Vehicle.Variant as Variant

data DriverQuoteStatus = Active | Inactive
  deriving (Show, Read)

data DriverQuote = DriverQuote
  { id :: Id DriverQuote,
    status :: DriverQuoteStatus,
    searchRequestId :: Id SearchRequest,
    driverId :: Id Person,
    driverName :: Text,
    driverRating :: Maybe Double,
    baseFare :: Double,
    extraFareSelected :: Maybe Double,
    vehicleVariant :: Variant.Variant,
    distanceToPickup :: Meters,
    durationToPickup :: Seconds,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    validTill :: UTCTime
  }
