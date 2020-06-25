{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.MapSearch
  ( module Beckn.Types.MapSearch,
    module Data.Geospatial,
  )
where

import Data.Generics.Labels
import Data.Geospatial
import Data.Time.LocalTime
import EulerHS.Prelude

data MapPoint
  = Place Text
  | LatLong PointXY
  | PlaceId Text
  deriving (Show)

data TravelMode = CAR | MOTORCYCLE | BICYCLE | FOOT
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Route = Route
  { distance :: Float,
    duration :: Integer,
    boundingBox :: Maybe BoundingBoxWithoutCRS,
    snapped_waypoints :: Maybe GeospatialGeometry,
    mode :: TravelMode,
    points :: Maybe GeospatialGeometry
  }
  deriving (Generic, ToJSON, FromJSON, Show)

data Request = Request
  { waypoints :: [MapPoint],
    mode :: Maybe TravelMode, -- Defaults to CAR
    departureTime :: Maybe LocalTime, -- optionally required for Transit mode which is not supported currently
    arrivalTime :: Maybe LocalTime, -- optionally required for Transit mode which is not supported currently
    calcPoints :: Maybe Bool -- True (default) if points needs to be calculated
  }
  deriving (Show)

data Response = Response
  { status :: Text,
    routes :: [Route]
  }
  deriving (Generic, ToJSON, FromJSON, Show)
