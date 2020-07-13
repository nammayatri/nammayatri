{-# LANGUAGE OverloadedLabels #-}

module Product.Location where

import App.Types
import qualified Beckn.Product.MapSearch as MapSearch
import Beckn.Types.App
import Beckn.Types.MapSearch (BoundingBoxWithoutCRS, GeospatialGeometry, PointXY (..))
import qualified Beckn.Types.MapSearch as MapSearch
import qualified Beckn.Types.Storage.Location as Location
import qualified Beckn.Types.Storage.RegistrationToken as SR
import Beckn.Utils.Common
import Beckn.Utils.Extra (getCurrentTimeUTC)
import Data.Generics.Labels
import Data.Time.Clock (nominalDiffTimeToSeconds)
import Data.Time.LocalTime
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (state)
import Servant
import qualified Storage.Queries.Case as Case
import qualified Storage.Queries.Location as Location
import qualified Storage.Queries.RegistrationToken as QR
import qualified Storage.Redis.Queries as Redis
import Types.API.Location as Location

data CachedLocationInfo = CachedLocationInfo
  { locationType :: Maybe Location.LocationType,
    lat :: Maybe Double,
    long :: Maybe Double,
    traversed_waypoints :: [(LocalTime, (Double, Double))],
    destLat :: Maybe Double,
    destLong :: Maybe Double,
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
    snapped_waypoints :: Maybe GeospatialGeometry
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
      let locInfo = createLocationInfo req
      case_ <- Case.findById $ CaseId caseId
      fromLocation <- Location.findLocationById (LocationId $ case_ ^. #_fromLocationId)
      loc <-
        case (fromLocation ^. #_lat, fromLocation ^. #_long) of
          (Just custLat, Just custLong) -> do
            -- Get route and eta
            route <- getRoute' driverLat driverLon custLat custLong
            createLocationInfo req (Just (custLat, custLong)) route
          _ -> do
            L.logInfo "GetRoute" "Lat,Long for fromLocation not found"
            createLocationInfo req Nothing Nothing
      Redis.setKeyRedis caseId loc
    Just (loc :: CachedLocationInfo) -> do
      updatedLoc <- updateLocationInfo req Nothing loc
      Redis.setKeyRedis caseId updatedLoc
  return $ UpdateLocationRes "SUCCESS"

getLocation :: Text -> FlowHandler GetLocationRes
getLocation caseId = withFlowHandler $ do
  (GetLocationRes . map fromCacheLocationInfo) <$> Redis.getKeyRedis caseId

updateLocationInfo :: UpdateLocationReq -> Maybe MapSearch.Route -> CachedLocationInfo -> Flow CachedLocationInfo
updateLocationInfo UpdateLocationReq {..} routeM currLocInfo = do
  now <- getCurrentTimeUTC
  -- lat long will be present
  let lat' = fromJust lat
      long' = fromJust long
      waypointList = appendToWaypointList (now, (lat', long')) $ currLocInfo ^. #traversed_waypoints
      durationInS' = (getDurationInSeconds <$> routeM) <|> (currLocInfo ^. #durationInS)
      distanceInM' = (MapSearch.distanceInM <$> routeM) <|> (currLocInfo ^. #distanceInM)
      destLat = currLocInfo ^. #destLat
      destLong = currLocInfo ^. #destLong
      updatedDuration = updateDuration destLat destLong durationInS' waypointList
  return $
    CachedLocationInfo
      { locationType = locationType <|> (currLocInfo ^. #locationType),
        lat = Just lat',
        long = Just long',
        traversed_waypoints = waypointList,
        destLat,
        destLong,
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
        snapped_waypoints = (MapSearch.snapped_waypoints =<< routeM) <|> (currLocInfo ^. #snapped_waypoints)
      }
  where
    getDurationInSeconds route = div (MapSearch.durationInMS route) 1000 -- convert miliseconds to seconds
    updateDuration toLatM toLongM durationM waypointList =
      case (durationM, toLatM, toLongM) of
        (Just durationInS, Just destLat, Just destLong) -> Just $ calculateRemainingDuration waypointList (destLat, destLong) durationInS
        _ -> durationM

appendToWaypointList :: (LocalTime, (Double, Double)) -> [(LocalTime, (Double, Double))] -> [(LocalTime, (Double, Double))]
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

calculateRemainingDuration :: [(LocalTime, (Double, Double))] -> (Double, Double) -> Integer -> Integer
calculateRemainingDuration traversedWaypoints (destLat, destLong) initalDuration
  -- initialDuration is in seconds
  | length traversedWaypoints < 2 = initalDuration
  | otherwise =
    let edges = zip traversedWaypoints (drop 1 traversedWaypoints)
        speeds = calcSpeed <$> edges
        avgSpeed = (sum speeds) / (fromIntegral $ length speeds)
        latestWaypoint = last traversedWaypoints
        (lat, long) = snd latestWaypoint
        distanceToDestination = MapSearch.distanceBetweenInMeters (PointXY lat long) (PointXY destLat destLong)
     in if avgSpeed == 0 then 0 else round $ distanceToDestination / avgSpeed
  where
    calcSpeed ((t1, (lat1, lon1)), (t2, (lat2, lon2))) =
      let durationInS = abs $ round $ nominalDiffTimeToSeconds $ diffLocalTime t2 t1
          distanceInM = MapSearch.distanceBetweenInMeters (PointXY lat1 lon1) (PointXY lat2 lon2)
       in MapSearch.speedInMPS distanceInM durationInS

createLocationInfo :: UpdateLocationReq -> Maybe (Double, Double) -> Maybe MapSearch.Route -> Flow CachedLocationInfo
createLocationInfo UpdateLocationReq {..} destinationM routeM = do
  now <- getCurrentTimeUTC
  -- lat long will be present
  let lat' = fromJust lat
      long' = fromJust long
  return $
    CachedLocationInfo
      { locationType,
        lat = Just lat',
        long = Just long',
        traversed_waypoints = [(now, (lat', long'))],
        destLat = fst <$> destinationM,
        destLong = snd <$> destinationM,
        ward,
        district,
        city,
        state,
        country,
        pincode,
        address,
        durationInS = getDurationInSeconds <$> routeM,
        distanceInM = MapSearch.distanceInM <$> routeM,
        bbox = MapSearch.boundingBox =<< routeM,
        waypoints = MapSearch.points =<< routeM,
        snapped_waypoints = MapSearch.snapped_waypoints =<< routeM
      }
  where
    getDurationInSeconds route = div (MapSearch.durationInMS route) 1000 -- convert miliseconds to seconds

getRoute' :: Double -> Double -> Double -> Double -> Flow (Maybe MapSearch.Route)
getRoute' fromLat fromLon toLat toLon = do
  routeE <- MapSearch.getRoute getRouteRequest
  case routeE of
    Left err -> do
      L.logInfo "GetRoute" (show err)
      return Nothing
    Right MapSearch.Response {..} ->
      if null routes
        then return . Just $ head routes
        else return Nothing
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
