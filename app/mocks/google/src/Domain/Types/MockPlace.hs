{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Domain.Types.MockPlace where

import Beckn.External.Maps.Google.MapsClient
import qualified Beckn.External.Maps.Google.RoadsClient as Roads
import Beckn.Prelude
import Control.Applicative
import qualified Data.Map as Map

type SnapToRoadResponse = Roads.SnapToRoadResponse' Roads.SPLocation

newtype PlaceId = PlaceId Int deriving newtype (Eq, Ord, Num)

type PlaceName = Text

data MockPlace = MockPlace
  { placeId :: PlaceId,
    place :: Place,
    placeName :: PlaceName
  }

lookupPlace :: Place -> Map.Map PlaceId MockPlace -> Maybe MockPlace
lookupPlace place = find (\mockPlace -> comparePlaces place mockPlace.place)

lookupDistanceMatrixElement :: (PlaceId, PlaceId) -> Map.Map (PlaceId, PlaceId) DistanceMatrixElement -> Maybe DistanceMatrixElement
lookupDistanceMatrixElement (point1, point2) distanceMatrixElements =
  Map.lookup (point1, point2) distanceMatrixElements <|> Map.lookup (point2, point1) distanceMatrixElements

comparePlaces :: Place -> Place -> Bool
comparePlaces (Address address1) (Address address2) = address1 == address2
comparePlaces (Location location1) (Location location2) =
  equalsEps location1.lat location2.lat && equalsEps location1.lng location2.lng
comparePlaces _ _ = False

equalsEps :: Double -> Double -> Bool
equalsEps x y = abs (x - y) < eps

-- test cases: route7.origin.lat=14.445332, route8.origin.lat=14.445352
eps :: Double
eps = 0.000021
