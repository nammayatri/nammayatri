{-# LANGUAGE OverloadedLabels #-}

module Product.Location where

import qualified Beckn.Product.MapSearch as MapSearch
import Beckn.Types.App
import qualified Beckn.Types.MapSearch as MapSearch
import qualified Beckn.Types.Storage.Location as Location
import Beckn.Utils.Common
import Data.Generics.Labels
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (state)
import Servant
import qualified Storage.Queries.Case as Case
import qualified Storage.Queries.Location as Location
import qualified Storage.Queries.RegistrationToken as QR
import qualified Storage.Redis.Queries as Redis
import Types.API.Location

updateLocation :: Text -> RegToken -> UpdateLocationReq -> FlowHandler UpdateLocationRes
updateLocation caseId regToken req = withFlowHandler $ do
  QR.verifyToken regToken -- TODO: Move this verification to redis
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
            route <- getRoute driverLat driverLon custLat custLong
            return $ createLocationInfo req route
          _ -> do
            L.logInfo "GetRoute" "Lat,Long for fromLocation not found"
            return $ createLocationInfo req Nothing
      Redis.setKeyRedis caseId loc
    Just (loc :: LocationInfo) -> do
      let updatedLoc = updateLocationInfo req Nothing loc
      Redis.setKeyRedis caseId updatedLoc
  return $ UpdateLocationRes "SUCCESS"

getLocation :: Text -> FlowHandler GetLocationRes
getLocation caseId = withFlowHandler $ do
  GetLocationRes <$> (Redis.getKeyRedis caseId)

updateLocationInfo :: UpdateLocationReq -> Maybe MapSearch.Route -> LocationInfo -> LocationInfo
updateLocationInfo UpdateLocationReq {..} routeM currLocInfo =
  currLocInfo
    { locationType = locationType <|> (currLocInfo ^. #locationType),
      lat = lat <|> (currLocInfo ^. #lat),
      long = long <|> (currLocInfo ^. #long),
      ward = ward <|> (currLocInfo ^. #ward),
      district = district <|> (currLocInfo ^. #district),
      city = city <|> (currLocInfo ^. #city),
      state = state <|> (currLocInfo ^. #state),
      country = country <|> (currLocInfo ^. #country),
      pincode = pincode <|> (currLocInfo ^. #pincode),
      address = address <|> (currLocInfo ^. #address),
      duration = (MapSearch.duration <$> routeM) <|> (currLocInfo ^. #duration),
      distance = (MapSearch.distance <$> routeM) <|> (currLocInfo ^. #distance),
      bbox = (MapSearch.boundingBox =<< routeM) <|> (currLocInfo ^. #bbox),
      waypoints = (MapSearch.points =<< routeM) <|> (currLocInfo ^. #waypoints),
      snapped_waypoints = (MapSearch.snapped_waypoints =<< routeM) <|> (currLocInfo ^. #snapped_waypoints)
    }

createLocationInfo :: UpdateLocationReq -> Maybe MapSearch.Route -> LocationInfo
createLocationInfo UpdateLocationReq {..} routeM =
  LocationInfo
    { locationType,
      lat,
      long,
      ward,
      district,
      city,
      state,
      country,
      pincode,
      address,
      duration = MapSearch.duration <$> routeM,
      distance = MapSearch.distance <$> routeM,
      bbox = MapSearch.boundingBox =<< routeM,
      waypoints = MapSearch.points =<< routeM,
      snapped_waypoints = MapSearch.snapped_waypoints =<< routeM
    }

getRoute :: Double -> Double -> Double -> Double -> L.Flow (Maybe MapSearch.Route)
getRoute fromLat fromLon toLat toLon = do
  routeE <- MapSearch.getRoute $ getRouteRequest
  case routeE of
    Left err -> do
      L.logInfo "GetRoute" (show err)
      return Nothing
    Right MapSearch.Response {..} ->
      if length routes > 0
        then return $ Just $ head routes
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
