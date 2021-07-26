{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.MapSearch
  ( module Beckn.Types.MapSearch,
    module Data.Geospatial,
    module Data.LineString,
  )
where

import Data.Geospatial
import Data.LineString
import EulerHS.Prelude

data LatLong = LatLong
  { lat :: Double,
    lon :: Double
  }
  deriving (Show, Generic, FromJSON, ToJSON)

data TravelMode = CAR | MOTORCYCLE | BICYCLE | FOOT
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Route = Route
  { durationInS :: Integer,
    distanceInM :: Double,
    boundingBox :: Maybe BoundingBoxWithoutCRS,
    snappedWaypoints :: GeospatialGeometry,
    points :: Maybe GeospatialGeometry
  }
  deriving (Generic, ToJSON, FromJSON, Show)

data Request = Request
  { waypoints :: [LatLong],
    mode :: Maybe TravelMode, -- Defaults to CAR
    calcPoints :: Bool -- True (default) if points needs to be calculated
  }
  deriving (Generic, ToJSON, FromJSON, Show)

type Response = [Route]
