{-# LANGUAGE OverloadedLabels #-}

module Beckn.Product.MapSearch where

import qualified Beckn.External.Graphhopper.Flow as Grphr
import qualified Beckn.External.Graphhopper.Types as Grphr
import qualified Beckn.Types.MapSearch as MapSearch
import Data.Geospatial
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Servant

getRoute :: MapSearch.Request -> L.Flow (Either SomeException (MapSearch.Response))
getRoute MapSearch.Request {..} = do
  -- Currently integrated only with graphhopper
  case all isLatLong waypoints of
    False -> return $ Left $ toException $ err400 {errBody = "Not supporting waypoints other than LatLong."}
    True -> do
      let points = map (\(MapSearch.LatLong point) -> point) waypoints
          mode' = fromMaybe MapSearch.CAR mode
          vehicle = mapToVehicle mode'
      res <- Grphr.search Grphr.defaultGrphrBaseUrl (grphrReq points vehicle)
      case res of
        Left err -> return $ Left $ toException err
        Right (Grphr.Response {..}) -> do
          return $ Right $
            MapSearch.Response
              { status = "OK",
                routes = (mapToRoute mode') <$> _paths
              }
  where
    isLatLong :: MapSearch.MapPoint -> Bool
    isLatLong (MapSearch.LatLong _) = True
    isLatLong _ = False
    grphrReq :: [PointXY] -> Grphr.Vehicle -> Grphr.Request
    grphrReq points vehicle =
      Grphr.Request
        { _points' = points,
          _vehicle = vehicle,
          _weighting = Nothing,
          _elevation = Nothing,
          _calcPoints = calcPoints
        }

mapToVehicle :: MapSearch.TravelMode -> Grphr.Vehicle
mapToVehicle MapSearch.CAR = Grphr.CAR
mapToVehicle MapSearch.MOTORCYCLE = Grphr.SCOOTER
mapToVehicle MapSearch.BICYCLE = Grphr.BIKE
mapToVehicle MapSearch.FOOT = Grphr.FOOT

mapToRoute :: MapSearch.TravelMode -> Grphr.Path -> MapSearch.Route
mapToRoute mode Grphr.Path {..} =
  MapSearch.Route
    { distance = _distance,
      duration = _time,
      boundingBox = _bbox,
      snapped_waypoints = Just _snapped_waypoints,
      mode = mode,
      points = _points
    }
