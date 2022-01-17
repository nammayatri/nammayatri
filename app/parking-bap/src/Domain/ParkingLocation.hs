{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.ParkingLocation where

import Beckn.Prelude
import Beckn.Types.Id

data ParkingLocation = ParkingLocation
  { id :: Id ParkingLocation,
    idFromBpp :: Text,
    lat :: Double,
    lon :: Double,
    name :: Text,
    streetAddress :: Text,
    locality :: Text,
    city :: Maybe Text,
    state :: Text,
    country :: Text,
    areaCode :: Text,
    createdAt :: UTCTime
  }
  deriving (Generic)

data ParkingLocationAPIEntity = ParkingLocationAPIEntity
  { lat :: Double,
    lon :: Double,
    name :: Text,
    streetAddress :: Text,
    locality :: Text,
    city :: Maybe Text,
    state :: Text,
    country :: Text,
    areaCode :: Text
  }
  deriving (Generic, ToJSON, ToSchema)

makeParkingLocationAPIEntity :: ParkingLocation -> ParkingLocationAPIEntity
makeParkingLocationAPIEntity ParkingLocation {..} =
  ParkingLocationAPIEntity {..}
