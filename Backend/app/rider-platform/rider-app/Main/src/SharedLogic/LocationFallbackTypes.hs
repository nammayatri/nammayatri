module SharedLogic.LocationFallbackTypes
  ( BppLocation (..),
    BppBookingLocationsRes (..),
  )
where

import Kernel.Prelude

data BppLocation = BppLocation
  { lat :: Double,
    lon :: Double,
    street :: Maybe Text,
    door :: Maybe Text,
    city :: Maybe Text,
    state :: Maybe Text,
    country :: Maybe Text,
    building :: Maybe Text,
    areaCode :: Maybe Text,
    area :: Maybe Text,
    instructions :: Maybe Text,
    extras :: Maybe Text
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

data BppBookingLocationsRes = BppBookingLocationsRes
  { from :: BppLocation,
    to :: Maybe BppLocation,
    stops :: [BppLocation]
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)
