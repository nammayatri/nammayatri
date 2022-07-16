{-# LANGUAGE DerivingVia #-}

module Domain.Types.DriverQuote where

import Beckn.Prelude
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Utils.GenericPretty
import qualified Domain.Types.FareParams as Params
import Domain.Types.Person
import Domain.Types.SearchRequest
import qualified Domain.Types.Vehicle.Variant as Variant

data DriverQuoteStatus = Active | Inactive
  deriving (Show, Read)
  deriving (PrettyShow) via Showable DriverQuoteStatus

data DriverQuote = DriverQuote
  { id :: Id DriverQuote,
    status :: DriverQuoteStatus,
    searchRequestId :: Id SearchRequest,
    driverId :: Id Person,
    driverName :: Text,
    driverRating :: Maybe Double,
    vehicleVariant :: Variant.Variant,
    distance :: Double,
    distanceToPickup :: Meters,
    durationToPickup :: Seconds,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    validTill :: UTCTime,
    fareParams :: Params.FareParameters
  }
  deriving (Generic, Show, PrettyShow)
