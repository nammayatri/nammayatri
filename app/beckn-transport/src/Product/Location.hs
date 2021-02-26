{-# LANGUAGE OverloadedLabels #-}

module Product.Location where

import App.Types
import qualified Beckn.Product.MapSearch as MapSearch
import Beckn.Types.Id
import qualified Beckn.Types.MapSearch as MapSearch
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Location as Location
import qualified Beckn.Types.Storage.Person as Person
import qualified Beckn.Types.Storage.RegistrationToken as SR
import Beckn.Utils.Common
import EulerHS.Prelude hiding (state)
import qualified Storage.Queries.Location as Location
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.ProductInstance as ProductInstance
import Types.API.Location as Location

updateLocation :: SR.RegistrationToken -> UpdateLocationReq -> FlowHandler UpdateLocationRes
updateLocation SR.RegistrationToken {..} req = withFlowHandler $ do
  person <- Person.findPersonById $ Id _EntityId
  driver <- if person ^. #_role == Person.DRIVER then return person else throwErrorMsg400 "INVALID_ROLE" "Only driver can update location."
  locationId <-
    driver ^. #_locationId & (Id <$>)
      & fromMaybeMWithMsg500 "LOCATION_NOT_FOUND" "Driver location not found"
  Location.updateGpsCoord locationId (req ^. #lat) (req ^. #long)
  return $ UpdateLocationRes "ACK"

getLocation :: Text -> FlowHandler GetLocationRes
getLocation piId = withFlowHandler $ do
  orderProductInstance <- ProductInstance.findByParentIdType (Just $ Id piId) Case.RIDEORDER
  driver <-
    orderProductInstance ^. #_personId & fromMaybeM400 "DRIVER_NOT_ASSIGNED"
      >>= Person.findPersonById
  currLocation <-
    driver ^. #_locationId & (Id <$>)
      & fromMaybeMWithMsg500 "LOCATION_NOT_FOUND" "Driver location not found"
      >>= Location.findLocationById
      >>= fromMaybeMWithMsg500 "LOCATION_NOT_FOUND" "Driver location not found"
  lat <- currLocation ^. #_lat & fromMaybeM500 "GPS_COORDS_NOT_SET"
  long <- currLocation ^. #_long & fromMaybeM500 "GPS_COORDS_NOT_SET"
  return $ GetLocationRes {location = Location.LocationInfo lat long}

getRoute' :: Double -> Double -> Double -> Double -> Flow (Maybe MapSearch.Route)
getRoute' fromLat fromLon toLat toLon = do
  routeE <- MapSearch.getRoute getRouteRequest
  case routeE of
    Left err -> do
      logInfo "GetRoute" (show err)
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
        (throwErrorMsg400 "UNABLE_TO_GET_ROUTE" . show)
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

calculateDistance :: Location.Location -> Location.Location -> Flow (Maybe Float)
calculateDistance source destination = do
  routeRequest <- mkRouteRequest
  response <- MapSearch.getRoute routeRequest
  case response of
    Left err -> do
      logWarning "" $ "Failed to calculate distance. Reason: " +|| err ||+ ""
      pure Nothing
    Right result -> pure $ MapSearch.distanceInM <$> headMaybe (result ^. #routes)
  where
    mkRouteRequest = do
      sourceLat <- source ^. #_lat & fromMaybeM500 "LAT_NOT_FOUND"
      sourceLng <- source ^. #_long & fromMaybeM500 "LNG_NOT_FOUND"
      destinationLat <- destination ^. #_lat & fromMaybeM500 "LAT_NOT_FOUND"
      destinationLng <- destination ^. #_long & fromMaybeM500 "LNG_NOT_FOUND"
      let sourceMapPoint = mkMapPoint sourceLat sourceLng
      let destinationMapPoint = mkMapPoint destinationLat destinationLng
      pure $
        MapSearch.Request
          { waypoints = [sourceMapPoint, destinationMapPoint],
            mode = Just MapSearch.CAR,
            departureTime = Nothing,
            arrivalTime = Nothing,
            calcPoints = Just False
          }
    mkMapPoint lat lng = MapSearch.LatLong $ MapSearch.PointXY lat lng
