module Beckn.Product.MapSearch where

import Beckn.External.GoogleMaps.Types as GoogleMaps
import Beckn.Prelude
import qualified Beckn.Product.MapSearch.GoogleMaps as GoogleMaps
import Beckn.Tools.Metrics.CoreMetrics (CoreMetrics)
import qualified Beckn.Types.MapSearch as MapSearch
import Beckn.Utils.Common hiding (id)

getDistance ::
  ( MonadFlow m,
    CoreMetrics m,
    HasGoogleMaps m r c
  ) =>
  Maybe MapSearch.TravelMode ->
  MapSearch.LatLong ->
  MapSearch.LatLong ->
  m GoogleMaps.GetDistanceResult
getDistance travelMode origin destination = do
  key <- asks (.googleMapsKey)
  case key of
    "mock-key" -> pure $ makeMockGetDistanceResult origin destination
    _ -> GoogleMaps.getDistance travelMode origin destination Nothing

getDistances ::
  ( MonadFlow m,
    CoreMetrics m,
    HasGoogleMaps m r c
  ) =>
  Maybe MapSearch.TravelMode ->
  [MapSearch.LatLong] ->
  [MapSearch.LatLong] ->
  m [GoogleMaps.GetDistanceResult]
getDistances travelMode origins destinations = do
  key <- asks (.googleMapsKey)
  case key of
    "mock-key" -> pure $ makeMockGetDistanceResult <$> origins <*> destinations
    _ -> GoogleMaps.getDistances travelMode origins destinations Nothing

-- FIXME Should we use some calculation here?

makeMockGetDistanceResult ::
  MapSearch.LatLong ->
  MapSearch.LatLong ->
  GoogleMaps.GetDistanceResult
makeMockGetDistanceResult origin dest =
  GoogleMaps.GetDistanceResult
    { origin = GoogleMaps.Location $ LocationS origin.lat origin.lon,
      destination = GoogleMaps.Location $ LocationS dest.lat dest.lon,
      info =
        GoogleMaps.GetDistanceResultInfo
          { distance = 9.446,
            duration = mockDuration,
            duration_in_traffic = mockDuration,
            status = "OK"
          }
    }
  where
    mockDuration = intToNominalDiffTime 648
