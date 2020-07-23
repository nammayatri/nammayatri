{-# LANGUAGE OverloadedLabels #-}

module Product.Location where

import App.Types
import qualified Beckn.Product.MapSearch as MapSearch
import Beckn.Types.App
import Beckn.Types.MapSearch (BoundingBoxWithoutCRS, GeoLine (..), GeospatialGeometry (..), PointXY (..), fromLineString, retrieveXY)
import qualified Beckn.Types.MapSearch as MapSearch
import qualified Beckn.Types.Storage.Location as Location
import qualified Beckn.Types.Storage.RegistrationToken as SR
import Beckn.Utils.Common
import Beckn.Utils.Extra (getCurrentTimeUTC)
import Data.Time.Clock (nominalDiffTimeToSeconds)
import Data.Time.LocalTime
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (state)
import Servant
import qualified Storage.Queries.Case as Case
import qualified Storage.Queries.Location as Location
import qualified Storage.Redis.Queries as Redis
import Types.API.Location as Location

data Waypoint = Waypoint {_lon :: Double, _lat :: Double} deriving (Show, Generic, ToJSON, FromJSON)

type Edge = (Waypoint, Waypoint)

type EdgeLength = Float

data CachedLocationInfo = CachedLocationInfo
  { locationType :: Maybe Location.LocationType,
    lat :: Maybe Double,
    long :: Maybe Double,
    traversed_waypoints :: [(LocalTime, Waypoint)],
    destLat :: Maybe Double,
    destLon :: Maybe Double,
    ward :: Maybe Text,
    district :: Maybe Text,
    city :: Maybe Text,
    state :: Maybe Text,
    country :: Maybe Text,
    pincode :: Maybe Text,
    address :: Maybe Text,
    durationInS :: Maybe Integer,
    distanceInM :: Maybe Float,
    bbox :: Maybe BoundingBoxWithoutCRS,
    waypoints :: Maybe GeospatialGeometry,
    snapped_waypoints :: Maybe GeospatialGeometry,
    edges :: [(EdgeLength, Edge)]
  }
  deriving (Generic, ToJSON, Show, FromJSON)

updateLocation :: SR.RegistrationToken -> Text -> UpdateLocationReq -> FlowHandler UpdateLocationRes
updateLocation _ caseId req = withFlowHandler $ do
  -- TODO: Add a driver and case check
  driverLat <- maybe (L.throwException $ err400 {errBody = "Lat not specified"}) return $ req ^. #lat
  driverLon <- maybe (L.throwException $ err400 {errBody = "Long not specified"}) return $ req ^. #long
  cacheM <- Redis.getKeyRedis caseId
  case cacheM of
    Nothing -> do
      -- TODO: run this in a thread
      case_ <- Case.findById $ CaseId caseId
      fromLocation <- Location.findLocationById (LocationId $ case_ ^. #_fromLocationId)
      loc <-
        case (fromLocation ^. #_lat, fromLocation ^. #_long) of
          (Just custLat, Just custLong) -> do
            -- Get route and eta
            route <- getRoute' driverLat driverLon custLat custLong
            createLocationInfo req (Just $ Waypoint custLong custLat) route
          _ -> do
            L.logInfo "GetRoute" "Lat,Long for fromLocation not found"
            createLocationInfo req Nothing Nothing
      Redis.setKeyRedis caseId loc
    Just (loc :: CachedLocationInfo) -> do
      updatedLoc <- updateLocationInfo req Nothing loc
      Redis.setKeyRedis caseId updatedLoc
  return $ UpdateLocationRes "SUCCESS"

getLocation :: Text -> FlowHandler GetLocationRes
getLocation caseId =
  withFlowHandler $
    GetLocationRes . map fromCacheLocationInfo <$> Redis.getKeyRedis caseId

updateLocationInfo :: UpdateLocationReq -> Maybe MapSearch.Route -> CachedLocationInfo -> Flow CachedLocationInfo
updateLocationInfo UpdateLocationReq {..} routeM currLocInfo = do
  now <- getCurrentTimeUTC
  -- lat long will be present
  let lat' = fromJust lat
      long' = fromJust long
      waypointList = appendToWaypointList (now, Waypoint long' lat') $ currLocInfo ^. #traversed_waypoints
      durationInS' = (MapSearch.durationInS <$> routeM) <|> (currLocInfo ^. #durationInS)
      distanceInM' = (MapSearch.distanceInM <$> routeM) <|> (currLocInfo ^. #distanceInM)
      destLat = currLocInfo ^. #destLat
      destLon = currLocInfo ^. #destLon
      edges' = currLocInfo ^. #edges
      updatedDuration = updateDuration destLat destLon durationInS' edges' waypointList
  return $
    CachedLocationInfo
      { locationType = locationType <|> (currLocInfo ^. #locationType),
        lat = Just lat',
        long = Just long',
        traversed_waypoints = waypointList,
        destLat,
        destLon,
        ward = ward <|> (currLocInfo ^. #ward),
        district = district <|> (currLocInfo ^. #district),
        city = city <|> (currLocInfo ^. #city),
        state = state <|> (currLocInfo ^. #state),
        country = country <|> (currLocInfo ^. #country),
        pincode = pincode <|> (currLocInfo ^. #pincode),
        address = address <|> (currLocInfo ^. #address),
        durationInS = updatedDuration,
        distanceInM = distanceInM',
        bbox = (MapSearch.boundingBox =<< routeM) <|> (currLocInfo ^. #bbox),
        waypoints = (MapSearch.points =<< routeM) <|> (currLocInfo ^. #waypoints),
        snapped_waypoints = (MapSearch.snapped_waypoints =<< routeM) <|> (currLocInfo ^. #snapped_waypoints),
        edges = edges'
      }
  where
    updateDuration toLatM toLongM durationM routeEdges waypointList =
      case (durationM, toLatM, toLongM) of
        (Just durationInS, Just destLat, Just destLon) -> Just $ calculateETA waypointList (Waypoint destLon destLat) routeEdges durationInS
        _ -> durationM

appendToWaypointList :: (LocalTime, Waypoint) -> [(LocalTime, Waypoint)] -> [(LocalTime, Waypoint)]
appendToWaypointList newLoc list = do
  -- number of waypoints using which average speed is calculated
  let totalWaypointsToTrack = 3 -- minimum 2 required
      len = length list
  if length list < totalWaypointsToTrack
    then list <> [newLoc]
    else
      let dropLen = len - totalWaypointsToTrack + 1
          list' = drop dropLen list
       in list' <> [newLoc]

calculateETA :: [(LocalTime, Waypoint)] -> Waypoint -> [(EdgeLength, Edge)] -> Integer -> Integer
calculateETA traversedWaypoints (Waypoint destLon destLat) routeEdges initalDuration
  -- initialDuration is in seconds
  | length traversedWaypoints < 2 = initalDuration
  | otherwise =
    let edges = zip traversedWaypoints (drop 1 traversedWaypoints)
        speeds = calcSpeed <$> edges
        avgSpeed = sum speeds / fromIntegral (length speeds)
        latestWaypoint = last traversedWaypoints
        (Waypoint long lat) = snd latestWaypoint
        dist =
          fromMaybe
            -- Fallback to using straight line distance if there are no edge intersection
            (MapSearch.distanceBetweenInMeters (PointXY long lat) (PointXY destLon destLat))
            (distanceToDestination (Waypoint long lat) routeEdges)
     in if avgSpeed == 0 then 0 else round $ dist / avgSpeed
  where
    calcSpeed ((t1, Waypoint lon1 lat1), (t2, Waypoint lon2 lat2)) =
      let durationInS = abs $ round $ nominalDiffTimeToSeconds $ diffLocalTime t2 t1
          distanceInM = MapSearch.distanceBetweenInMeters (PointXY lon1 lat1) (PointXY lon2 lat2)
       in MapSearch.speedInMPS distanceInM durationInS

createLocationInfo :: UpdateLocationReq -> Maybe Waypoint -> Maybe MapSearch.Route -> Flow CachedLocationInfo
createLocationInfo UpdateLocationReq {..} destinationM routeM = do
  now <- getCurrentTimeUTC
  -- lat long will be present
  let lat' = fromJust lat
      long' = fromJust long
      waypoints = MapSearch.points =<< routeM
  return $
    CachedLocationInfo
      { locationType,
        lat = Just lat',
        long = Just long',
        traversed_waypoints = [(now, Waypoint long' lat')],
        destLat = _lat <$> destinationM,
        destLon = _lon <$> destinationM,
        ward,
        district,
        city,
        state,
        country,
        pincode,
        address,
        durationInS = MapSearch.durationInS <$> routeM,
        distanceInM = MapSearch.distanceInM <$> routeM,
        bbox = MapSearch.boundingBox =<< routeM,
        waypoints,
        snapped_waypoints = MapSearch.snapped_waypoints =<< routeM,
        edges = concat $ maybeToList (calculateEdges <$> waypoints)
      }

getRoute' :: Double -> Double -> Double -> Double -> Flow (Maybe MapSearch.Route)
getRoute' fromLat fromLon toLat toLon = do
  routeE <- MapSearch.getRoute getRouteRequest
  case routeE of
    Left err -> do
      L.logInfo "GetRoute" (show err)
      return Nothing
    Right MapSearch.Response {..} ->
      pure $
        if null routes
          then Nothing
          else Just $ head routes
  where
    getRouteRequest = do
      let from = MapSearch.LatLong $ MapSearch.PointXY fromLat fromLon
      let to = MapSearch.LatLong $ MapSearch.PointXY toLat toLon
      MapSearch.Request
        { waypoints = [from, to],
          mode = Just MapSearch.CAR,
          departureTime = Nothing,
          arrivalTime = Nothing,
          calcPoints = Just True
        }

getRoute :: SR.RegistrationToken -> Location.Request -> FlowHandler Location.Response
getRoute _ Location.Request {..} =
  withFlowHandler $
    MapSearch.getRoute getRouteRequest
      >>= either
        (\err -> L.throwException $ err400 {errBody = show err})
        return
  where
    mapToMapPoint (Location.LatLong lat long) = MapSearch.LatLong $ MapSearch.PointXY lat long
    getRouteRequest =
      MapSearch.Request
        { waypoints = mapToMapPoint <$> waypoints,
          mode = mode <|> Just MapSearch.CAR,
          departureTime = Nothing,
          arrivalTime = Nothing,
          calcPoints
        }

fromCacheLocationInfo :: CachedLocationInfo -> LocationInfo
fromCacheLocationInfo CachedLocationInfo {..} = LocationInfo {..}

calculateEdges :: GeospatialGeometry -> [(EdgeLength, Edge)]
calculateEdges (Line geoLine) =
  let points =
        (\point -> Waypoint (_xyX point) (_xyY point)) . retrieveXY
          <$> fromLineString (_unGeoLine geoLine)
      edges = zip points (drop 1 points)
      edgeLengths = calcDistance <$> edges
   in zip edgeLengths edges
  where
    calcDistance :: Edge -> EdgeLength
    calcDistance (p1, p2) =
      let src = PointXY (_lon p1) (_lat p1)
          dest = PointXY (_lon p2) (_lat p2)
       in MapSearch.distanceBetweenInMeters src dest
calculateEdges _ = []

-- Traverse the edges and sum their length
traverseEdges :: [(EdgeLength, Edge)] -> Float
traverseEdges edges = sum $ fst <$> edges

isWithin ::
  [((Double, Double), (Double, Double))] ->
  (Double, Double) ->
  Bool
isWithin edges point =
  let intersections = doesIntersect point <$> edges
   in odd $ length $ filter (True ==) intersections

doesIntersect ::
  (Double, Double) ->
  ((Double, Double), (Double, Double)) ->
  Bool
doesIntersect (x, y) ((xi, yi), (xj, yj)) =
  let slope = (y - yi) / (yj - yi)
      dx = xj - xi
   in (y < yi) /= (y < yj) && (x < xi + dx * slope)

createBBox ::
  Double ->
  ((Double, Double), (Double, Double)) ->
  [((Double, Double), (Double, Double))]
createBBox width ((x1, y1), (x2, y2)) =
  let dx = x2 - x1
      dy = y2 - y1
      len = sqrt (dx * dx + dy * dy)
      px = width * (- dy / len)
      py = width * (dx / len)
      p1 = (x1 + px, y1 + py)
      p2 = (x2 + px, y2 + py)
      p3 = (x2 - px, y2 - py)
      p4 = (x1 - px, y1 - py)
   in [ (p1, p2),
        (p2, p3),
        (p3, p4),
        (p4, p1)
      ]

type Counter = Int

type EdgeIndex = Int

findClosestEdge :: Waypoint -> (Counter, Maybe EdgeIndex) -> (EdgeLength, Edge) -> (Counter, Maybe EdgeIndex)
findClosestEdge (Waypoint currLon currLat) (ctr, edgeIdx) (_, edge) =
  -- Gap on either side of an edge for creating bbox
  let gap = 10 -- in meters
      (Waypoint lon1 lat1) = fst edge
      (Waypoint lon2 lat2) = snd edge
      -- Project lat,lon to x,y on a plane
      -- Only works for smaller area, so that area can be approximated as a plane
      -- phi0 is required for maintaining aspect ratio of the projection
      phi0 = (lon2 - lon1) / 2
      (x1, y1) = latLonToXY (lat1, lon1) phi0
      (x2, y2) = latLonToXY (lat2, lon2) phi0
      bbox = createBBox gap ((x1, y1), (x2, y2))
      currXY = latLonToXY (currLat, currLon) phi0
   in if isWithin bbox currXY
        then (ctr + 1, Just ctr)
        else (ctr + 1, edgeIdx)

distanceToDestination :: Waypoint -> [(EdgeLength, Edge)] -> Maybe Float
distanceToDestination currPoint routeEdges =
  -- Find the closest edge to the point
  let (_, mIntersectingEdgeIdx) = foldl (findClosestEdge currPoint) (0, Nothing) routeEdges
   in case mIntersectingEdgeIdx of
        Nothing -> Nothing
        Just edgeIdx ->
          -- sum the distance of the edges not traversed yet
          let remainingEdges = drop (edgeIdx + 1) routeEdges
              dist = traverseEdges remainingEdges
              -- distance from currPoint to end of the closest edge
              endPointOfEdge = snd $ snd (routeEdges !! edgeIdx)
              dist' = MapSearch.distanceBetweenInMeters (waypointToPointXY currPoint) (waypointToPointXY endPointOfEdge)
           in Just (dist + dist')

waypointToPointXY :: Waypoint -> PointXY
waypointToPointXY (Waypoint lon lat) = PointXY lon lat

latLonToXY :: (Double, Double) -> Double -> (Double, Double)
latLonToXY (lat, long) refLon =
  let r = 6371000 -- Radius of earth in meters
      cos_phi0 = cos $ MapSearch.deg2Rad refLon
      x = r * MapSearch.deg2Rad lat * cos_phi0
      y = r * MapSearch.deg2Rad long
   in (x, y)
