module Beckn.Product.MapSearch where

import Beckn.Prelude
import qualified Beckn.Product.MapSearch.GoogleMaps as GoogleMaps
import qualified Beckn.Types.MapSearch as MapSearch
import Beckn.Types.Monitoring.Prometheus.Metrics (CoreMetrics)
import Beckn.Utils.Common hiding (id)

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
  key <- asks (.googleMapsKey)
  case key of
    "mock-key" -> pure mockDistance
    _ -> GoogleMaps.getDistance travelMode origin destination

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
  key <- asks (.googleMapsKey)
  case key of
    "mock-key" -> pure (mockDistance, mockDuration)
    _ -> GoogleMaps.getDurationAndDistance travelMode origin destination utcDepartureTime

-- FIXME Should we use some calculation here?
mockDistance :: Double
mockDistance = 9.446

mockDuration :: NominalDiffTime
mockDuration = intToNominalDiffTime 648
