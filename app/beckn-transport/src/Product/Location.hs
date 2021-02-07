{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Product.Location where

import App.Types
import qualified Beckn.Product.MapSearch as MapSearch
import Beckn.Types.App
import qualified Beckn.Types.MapSearch as MapSearch
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Location as Location
import qualified Beckn.Types.Storage.RegistrationToken as SR
import Beckn.Utils.Common
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (state)
import qualified Models.Case as Case
import qualified Storage.Queries.Location as Location
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.ProductInstance as ProductInstance
import Types.API.Location as Location

updateLocation :: SR.RegistrationToken -> Text -> UpdateLocationReq -> FlowHandler UpdateLocationRes
updateLocation _ caseId req = withFlowHandler $ do
  orderC <-
    Case.findByParentCaseIdAndType (CaseId caseId) Case.RIDEORDER
      >>= fromMaybeM400 "Case not found"
  orderProductInstance <- ProductInstance.findByTypeCaseId Case.RIDEORDER (orderC ^. #_id)
  driver <-
    orderProductInstance ^. #_personId & fromMaybeM400 "Driver not assigned"
      >>= Person.findPersonById
  locationId <-
    driver ^. #_locationId & (LocationId <$>)
      & fromMaybeM500 "Driver location not found"
  Location.updateGpsCoord locationId (req ^. #lat) (req ^. #long)
  return $ UpdateLocationRes "ACK"

getLocation :: Text -> FlowHandler GetLocationRes
getLocation caseId = withFlowHandler $ do
  orderCase <-
    Case.findByParentCaseIdAndType (CaseId caseId) Case.RIDEORDER
      >>= fromMaybeM400 "Case not found"
  orderProductInstance <- ProductInstance.findByTypeCaseId Case.RIDEORDER (orderCase ^. #_id)
  driver <-
    orderProductInstance ^. #_personId & fromMaybeM400 "Driver not assigned"
      >>= Person.findPersonById
  currLocation <-
    driver ^. #_locationId & (LocationId <$>)
      & fromMaybeM500 "Driver location not found"
      >>= Location.findLocationById
      >>= fromMaybeM500 "Driver location not found"
  lat <- currLocation ^. #_lat & fromMaybeM500 "Gps coord not set"
  long <- currLocation ^. #_long & fromMaybeM500 "Gps coord not set"
  return $ GetLocationRes {location = Location.LocationInfo lat long}

getRoute' :: Double -> Double -> Double -> Double -> Flow (Maybe MapSearch.Route)
getRoute' fromLat fromLon toLat toLon = do
  routeE <- MapSearch.getRoute getRouteRequest

  case routeE of
    Left err -> do
      L.logInfo @Text "GetRoute" (show err)

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
        (throwError400 . show)
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
      L.logWarning @Text "" $ "Failed to calculate distance. Reason: " +|| err ||+ ""
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
