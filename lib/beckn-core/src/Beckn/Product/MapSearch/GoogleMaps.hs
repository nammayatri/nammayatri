module Beckn.Product.MapSearch.GoogleMaps
  ( getDistance,
    getDurationAndDistance,
  )
where

import qualified Beckn.External.GoogleMaps.Client as GoogleMaps
import qualified Beckn.External.GoogleMaps.Types as GoogleMaps
import Beckn.Prelude
import Beckn.Types.Common hiding (id)
import Beckn.Types.Error
import qualified Beckn.Types.MapSearch as MapSearch
import Beckn.Types.Monitoring.Prometheus.Metrics (CoreMetrics)
import Beckn.Utils.Common hiding (id)
import GHC.Float

getDistance ::
  ( CoreMetrics m,
    HasFlowEnv m r '["googleMapsUrl" ::: BaseUrl],
    HasFlowEnv m r '["googleMapsKey" ::: Text]
  ) =>
  Maybe MapSearch.TravelMode ->
  MapSearch.LatLong ->
  MapSearch.LatLong ->
  m Double
getDistance travelMode origin destination = do
  googleMapsUrl <- asks (.googleMapsUrl)
  key <- asks (.googleMapsKey)
  GoogleMaps.distanceMatrix googleMapsUrl origins destinations key Nothing mode
    >>= parseDistanceMatrixElement
    >>= parseDistance
  where
    origins = [latLongToPlace origin]
    destinations = [latLongToPlace destination]
    mode = mapToMode <$> travelMode

-- Caution: This request is using traffic information (durationInTraffic) and billed at a higher rate.
getDurationAndDistance ::
  ( CoreMetrics m,
    HasFlowEnv m r '["googleMapsUrl" ::: BaseUrl],
    HasFlowEnv m r '["googleMapsKey" ::: Text]
  ) =>
  Maybe MapSearch.TravelMode ->
  MapSearch.LatLong ->
  MapSearch.LatLong ->
  Maybe UTCTime ->
  m (Double, NominalDiffTime)
getDurationAndDistance travelMode origin destination utcDepartureTime = do
  googleMapsUrl <- asks (.googleMapsUrl)
  key <- asks (.googleMapsKey)
  let departureTime = case utcDepartureTime of
        Nothing -> Just GoogleMaps.Now
        Just time -> Just $ GoogleMaps.FutureTime time
  distanceMatrixElement <-
    GoogleMaps.distanceMatrix googleMapsUrl origins destinations key departureTime mode
      >>= parseDistanceMatrixElement
  distance <- parseDistance distanceMatrixElement
  durationInTraffic <- parseDurationInTraffic distanceMatrixElement
  pure (distance, durationInTraffic)
  where
    origins = [latLongToPlace origin]
    destinations = [latLongToPlace destination]
    mode = mapToMode <$> travelMode

parseDistanceMatrixElement :: (MonadThrow m, Log m) => GoogleMaps.DistanceMatrixResp -> m GoogleMaps.DistanceMatrixElement
parseDistanceMatrixElement distanceMatrixResp = do
  distanceMatrixRow <-
    (listToMaybe distanceMatrixResp.rows)
      & fromMaybeM (InternalError "No rows in distance matrix API response")
  (listToMaybe distanceMatrixRow.elements)
    & fromMaybeM (InternalError "No elements in distance matrix API response")
    >>= GoogleMaps.validateResponseStatus

parseDistance :: (MonadThrow m, Log m) => GoogleMaps.DistanceMatrixElement -> m Double
parseDistance distanceMatrixElement = do
  distance <-
    distanceMatrixElement.distance
      & fromMaybeM (InternalError "No distance value provided in distance matrix API response")
  pure $ int2Double distance.value / 1000

parseDurationInTraffic :: (MonadThrow m, Log m) => GoogleMaps.DistanceMatrixElement -> m NominalDiffTime
parseDurationInTraffic distanceMatrixElement = do
  durationInTraffic <-
    distanceMatrixElement.duration_in_traffic
      & fromMaybeM (InternalError "No duration_in_traffic value provided in distance matrix API response")
  pure $ intToNominalDiffTime durationInTraffic.value

latLongToPlace :: MapSearch.LatLong -> GoogleMaps.Place
latLongToPlace MapSearch.LatLong {..} =
  GoogleMaps.Location $ GoogleMaps.LocationS {lat = lat, lng = lon}

mapToMode :: MapSearch.TravelMode -> GoogleMaps.Mode
mapToMode MapSearch.CAR = GoogleMaps.DRIVING
mapToMode MapSearch.MOTORCYCLE = GoogleMaps.DRIVING
mapToMode MapSearch.BICYCLE = GoogleMaps.BICYCLING
mapToMode MapSearch.FOOT = GoogleMaps.WALKING
