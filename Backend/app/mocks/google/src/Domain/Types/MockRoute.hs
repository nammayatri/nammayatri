{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Domain.Types.MockRoute where

import qualified Domain.Types.MockPlace as DPlace
import Kernel.External.Maps.Google.MapsClient
import qualified Kernel.External.Maps.Google.RoadsClient as Roads
import Kernel.Prelude

type SnapToRoadResponse = Roads.SnapToRoadResponse' Roads.SPLocation

newtype RouteId = RouteId Int deriving newtype (Eq, Ord, Num, FromJSON, ToJSON, Show)

data MockRoute = MockRoute
  { routeId :: RouteId,
    route :: [LocationS]
  }
  deriving (Generic, FromJSON, ToJSON)

compareRoutes :: [Place] -> [Place] -> Bool
compareRoutes route1 route2 = (length route1 == length route2) && all (uncurry DPlace.comparePlaces) (zip route1 route2)

lookupRoute :: [LocationS] -> [MockRoute] -> Maybe RouteId
lookupRoute route1 = ((.routeId) <$>) . find (\MockRoute {route} -> compareRoutes (Location <$> route) (Location <$> route1))
