module Domain.Action.UI.GoogleMaps
  ( GoogleMaps.SearchLocationResp (..),
    GoogleMaps.PlaceDetailsResp (..),
    GoogleMaps.GetPlaceNameResp (..),
    autoComplete,
    placeDetails,
    getPlaceName,
  )
where

import qualified Beckn.External.GoogleMaps.Client as ClientGoogleMaps
import qualified Beckn.External.GoogleMaps.Types as GoogleMaps
import Beckn.Utils.Common (MonadFlow)
import EulerHS.Prelude
import Tools.Metrics

autoComplete :: (MonadFlow m, GoogleMaps.HasGoogleMaps m r, CoreMetrics m) => Maybe Text -> Text -> Text -> Integer -> Text -> m GoogleMaps.SearchLocationResp
autoComplete sessiontoken input location radius lang = do
  url <- asks (.googleMapsUrl)
  apiKey <- asks (.googleMapsKey)
  let components = "country:in"
  ClientGoogleMaps.autoComplete url apiKey input sessiontoken location radius components lang

placeDetails :: (MonadFlow m, GoogleMaps.HasGoogleMaps m r, CoreMetrics m) => Maybe Text -> Text -> m GoogleMaps.PlaceDetailsResp
placeDetails sessiontoken placeId = do
  url <- asks (.googleMapsUrl)
  apiKey <- asks (.googleMapsKey)
  let fields = "geometry"
  ClientGoogleMaps.placeDetails url sessiontoken apiKey placeId fields

getPlaceName ::
  (MonadFlow m, GoogleMaps.HasGoogleMaps m r, CoreMetrics m) =>
  Maybe Text ->
  Text ->
  Maybe Text ->
  m GoogleMaps.GetPlaceNameResp
getPlaceName sessiontoken latLng lang = do
  url <- asks (.googleMapsUrl)
  apiKey <- asks (.googleMapsKey)
  ClientGoogleMaps.getPlaceName url sessiontoken latLng apiKey $ GoogleMaps.toMbLanguage lang
