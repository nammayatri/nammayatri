{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}

module Lib.Maps.OSRM.RoadsClient where

import Data.Aeson
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Vector as V
import EulerHS.Prelude ((...))
import qualified EulerHS.Types as Euler
import Kernel.Prelude hiding (unlines)
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.App
import Kernel.Types.CommonImport
import Kernel.Types.Error
import Kernel.Utils.Common
import Kernel.Utils.GenericPretty
import Servant hiding (throwError)

-- types

type MatchAPI =
  "match"
    :> "v1"
    :> "car"
    :> Capture "coordinates" PointsList
    :> MandatoryQueryParam "tidy" AlwaysTrue
    :> QueryParam "radiuses" RadiusesList
    :> MandatoryQueryParam "geometries" GeometryRespType
    :> Get '[JSON] MatchResp

type TableAPI =
  "table"
    :> "v1"
    :> "driving"
    :> Capture "coordinates" PointsList
    :> MandatoryQueryParam "annotations" String
    :> MandatoryQueryParam "sources" SourcesList
    :> MandatoryQueryParam "destinations" DestinationsList
    :> Get '[JSON] OSRMTableResponse

type RouteAPI =
  "route"
    :> "v1"
    :> "driving"
    :> Capture "coordinates" PointsList
    :> MandatoryQueryParam "geometries" GeometryRespType
    :> MandatoryQueryParam "alternatives" Bool
    :> MandatoryQueryParam "steps" Bool
    :> Get '[JSON] OSRMRouteResponse

newtype PointsList = PointsList {getPointsList :: [LatLong]}

newtype SourcesList = SourcesList {getSourcesList :: [Int]}

newtype DestinationsList = DestinationsList {getDestinationsList :: [Int]}

instance ToHttpApiData PointsList where
  toUrlPiece (PointsList lst) = T.intercalate ";" $ map convertPoint lst

convertPoint :: LatLong -> Text
convertPoint (LatLong lat lon) = show lon <> "," <> show lat

data AlwaysTrue = AlwaysTrue

instance ToHttpApiData AlwaysTrue where
  toUrlPiece _ = "true"

newtype RadiusesList = RadiusesList {getRadiusesList :: [Int]}

instance ToHttpApiData RadiusesList where
  toUrlPiece (RadiusesList lst) = T.intercalate ";" $ map show lst

instance ToHttpApiData DestinationsList where
  toUrlPiece (DestinationsList lst) = T.intercalate ";" $ map show lst

instance ToHttpApiData SourcesList where
  toUrlPiece (SourcesList lst) = T.intercalate ";" $ map show lst

data GeometryRespType = GeoJson

instance ToHttpApiData GeometryRespType where
  toUrlPiece GeoJson = "geojson"

data OSRMTableResponse = OSRMTableResponse
  { distances :: [[Double]],
    code :: String,
    durations :: [[Double]]
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

data MatchResp = MatchResp
  { matchings :: NonEmpty Route,
    tracepoints :: [Maybe Waypoint]
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON, PrettyShow)

data Route = Route
  { distance :: Double, -- meters
    duration :: Double, -- seconds
    legs :: [RouteLeg],
    confidence :: Double, -- from 0 to 1, 1 is very confident
    geometry :: RouteGeometry
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON, PrettyShow)

newtype RouteGeometry = RouteGeometry
  { coordinates :: [Location]
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON, PrettyShow)

data RouteLeg = RouteLeg
  { distance :: Double, -- meters
    duration :: Double -- seconds
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON, PrettyShow)

data Waypoint = Waypoint
  { alternatives_count :: Int,
    waypoint_index :: Int,
    matchings_index :: Int,
    location :: Location
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON, PrettyShow)

newtype Location = Location {getLatLong :: LatLong}
  deriving stock (Generic, Show, Eq)
  deriving (PrettyShow) via (Showable Location)

data OSRMRouteResponse = OSRMRouteResponse
  { routes :: [OSRMRouteRoutes],
    waypoints :: [OSRMRouteWaypoint]
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

data OSRMRouteRoutes = OSRMRouteRoutes
  { geometry :: RouteGeometry,
    legs :: [RouteResponseLeg],
    distance :: Double, -- meters
    duration :: Double, -- seconds
    weight :: Double
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

data RouteResponseLeg = RouteResponseLeg
  { steps :: [Steps],
    distance :: Double,
    duration :: Double,
    weight :: Double
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

data Steps = Steps 
  { geometry :: RouteGeometry,
    maneuver :: Maneuver
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

newtype Maneuver = Maneuver
  { location :: Location
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

data OSRMRouteWaypoint = OSRMRouteWaypoint
  { distance :: Double,
    location :: Location
  }
 deriving stock (Generic, Show, Eq)
 deriving anyclass (FromJSON, ToJSON)

instance FromJSON Location where
  parseJSON = withArray "array [lon, lat]" $ \arr_ -> case toList arr_ of
    [lonVal, latVal] -> Location ... LatLong <$> parseJSON latVal <*> parseJSON lonVal
    _ -> fail "expected array [lon, lat]"

instance ToJSON Location where
  toJSON (Location LatLong {..}) = Array $ V.fromList $ map toJSON [lon, lat]

-- call
callOsrmMatchAPI ::
  ( HasCallStack,
    Metrics.CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Maybe Int ->
  PointsList ->
  m MatchResp
callOsrmMatchAPI osrmUrl mbRadius pointsList = do
  let pointsNum = length pointsList.getPointsList
      radiuses = flip fmap mbRadius $ \r -> RadiusesList $ replicate pointsNum r
  let eulerClient = Euler.client (Proxy @MatchAPI)
  callAPI osrmUrl (eulerClient pointsList AlwaysTrue radiuses GeoJson) "osrm-match"
    >>= fromEitherM (\err -> InternalError $ "Failed to call osrm match API: " <> show err)

getResultOneRouteExpected :: (Log m, MonadThrow m) => MatchResp -> m (HighPrecMeters, [LatLong])
getResultOneRouteExpected resp = do
  logDebug $ T.pack $ defaultPretty resp
  route_ <- case NE.tail resp.matchings of
    [] -> pure $ NE.head resp.matchings
    (_ : _) -> throwError $ InternalError "OSRM failed to consider waypoints as part of one route, th result contains splitted routes"
  let points = map (.getLatLong) $ route_.geometry.coordinates
  pure (realToFrac route_.distance, points)

callOsrmGetDistancesAPI ::
  ( HasCallStack,
    Metrics.CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  PointsList ->
  SourcesList ->
  DestinationsList ->
  m OSRMTableResponse
callOsrmGetDistancesAPI osrmUrl pointsList sourcesList destinationsList =
  do
    let eulerClient = Euler.client (Proxy @TableAPI)
    callAPI osrmUrl (eulerClient pointsList "distance,duration" sourcesList destinationsList) "osrm-table"
    >>= fromEitherM (\err -> InternalError $ "Failed to call osrm table API: " <> show err)

callOsrmRouteAPI ::
  ( HasCallStack,
    Metrics.CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  PointsList ->
  m OSRMRouteResponse
callOsrmRouteAPI osrmUrl pointsList = do
  let eulerClient = Euler.client (Proxy @RouteAPI)
  callAPI osrmUrl (eulerClient pointsList GeoJson True True) "osrm-route"
    >>= fromEitherM (\err -> InternalError $ "Failed to call osrm route API: " <> show err)
