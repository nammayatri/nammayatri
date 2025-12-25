{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Domain.Types.MockPlace where

import Control.Applicative
import qualified Data.Map as Map
import Kernel.External.Maps.Google.MapsClient
import qualified Kernel.External.Maps.Google.RoadsClient as Roads
import Kernel.Prelude

type SnapToRoadResponse = Roads.SnapToRoadResponse' Roads.SPLocation

type SnappedPoint = Roads.SnappedPoint' Roads.SPLocation

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
