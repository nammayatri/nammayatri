{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Types.SearchRequestForDriver where

import Beckn.Prelude
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Utils.GenericPretty (PrettyShow)
import Domain.Types.Person
import Domain.Types.SearchRequest
import qualified Domain.Types.Vehicle.Variant as Variant

data SearchRequestForDriver = SearchRequestForDriver
  { id :: Id SearchRequestForDriver,
    searchRequestId :: Id SearchRequest,
    searchRequestValidTill :: UTCTime,
    driverId :: Id Person,
    distanceToPickup :: Meters,
    durationToPickup :: Seconds,
    vehicleVariant :: Variant.Variant,
    baseFare :: Double,
    createdAt :: UTCTime
  }
  deriving (Generic, Show, PrettyShow)

deriving newtype instance PrettyShow Seconds

deriving newtype instance PrettyShow Meters

data SearchRequestForDriverAPIEntity = SearchRequestForDriverAPIEntity
  { searchRequestId :: Id SearchRequest,
    searchRequestValidTill :: UTCTime,
    distanceToPickup :: Meters,
    durationToPickup :: Seconds,
    baseFare :: Double
  }
  deriving (Generic, ToJSON, ToSchema)

deriving newtype instance ToSchema Seconds

deriving newtype instance ToSchema Meters

mkSearchRequestForDriverAPIEntity :: SearchRequestForDriver -> SearchRequestForDriverAPIEntity
mkSearchRequestForDriverAPIEntity SearchRequestForDriver {..} = SearchRequestForDriverAPIEntity {..}
