{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.MapSearch
  ( module Beckn.Types.MapSearch,
    module Data.Geospatial,
    module Data.LineString,
  )
where

import Data.Geospatial
import Data.LineString
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
  { distanceInM :: Float, -- meters
    durationInS :: Integer, -- seconds
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
