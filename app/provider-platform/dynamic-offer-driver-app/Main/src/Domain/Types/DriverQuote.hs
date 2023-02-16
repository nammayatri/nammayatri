{-# LANGUAGE DerivingVia #-}

module Domain.Types.DriverQuote where

import qualified Domain.Types.FareParameters as Params
import Domain.Types.Person
import Domain.Types.SearchRequest
import Domain.Types.SearchRequestForDriver
import qualified Domain.Types.Vehicle.Variant as Variant
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.GenericPretty

data DriverQuoteStatus = Active | Inactive
  deriving (Show, Read, Eq)
  deriving (PrettyShow) via Showable DriverQuoteStatus

data DriverQuote = DriverQuote
  { id :: Id DriverQuote,
    status :: DriverQuoteStatus,
    searchRequestId :: Id SearchRequest,
    searchRequestForDriverId :: Maybe (Id SearchRequestForDriver),
    driverId :: Id Person,
    driverName :: Text,
    driverRating :: Maybe Centesimal,
    vehicleVariant :: Variant.Variant,
    distance :: Meters,
    distanceToPickup :: Meters,
    durationToPickup :: Seconds,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    validTill :: UTCTime,
    estimatedFare :: Money,
    fareParams :: Params.FareParameters
  }
  deriving (Generic, Show, PrettyShow)
