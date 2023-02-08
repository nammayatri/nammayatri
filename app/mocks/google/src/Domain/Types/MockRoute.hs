{-# LANGUAGE DerivingStrategies #-}
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
