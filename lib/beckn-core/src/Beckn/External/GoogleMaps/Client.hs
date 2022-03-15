module Beckn.External.GoogleMaps.Client where

import qualified Beckn.External.GoogleMaps.API as API
import qualified Beckn.External.GoogleMaps.Types as GoogleMaps
import Beckn.Types.Common
import Beckn.Types.Error
import Beckn.Types.Monitoring.Prometheus.Metrics (CoreMetrics)
import Beckn.Utils.Common
import EulerHS.Prelude
import Servant.Client.Core (ClientError)

autoComplete ::
  ( CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  Text ->
  Text ->
  Integer ->
  Text ->
  Text ->
  m GoogleMaps.SearchLocationResp
autoComplete url apiKey input location radius components lang = do
  callAPI url (API.autoComplete apiKey input location radius components lang) "autoComplete"
    >>= checkGoogleMapsError url

placeDetails ::
  ( CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  Text ->
  Text ->
  m GoogleMaps.PlaceDetailsResp
placeDetails url apiKey placeId fields = do
  callAPI url (API.placeDetails apiKey placeId fields) "placeDetails"
    >>= checkGoogleMapsError url

getPlaceName ::
  ( CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  Text ->
  m GoogleMaps.GetPlaceNameResp
getPlaceName url latLng apiKey = do
  callAPI url (API.getPlaceName latLng apiKey) "getPlaceName"
    >>= checkGoogleMapsError url

distanceMatrix ::
  ( CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  [GoogleMaps.Place] ->
  [GoogleMaps.Place] ->
  Text ->
  Maybe GoogleMaps.DepartureTime ->
  Maybe GoogleMaps.Mode ->
  m GoogleMaps.DistanceMatrixResp
distanceMatrix url origins destinations key departureTime mode = do
  callAPI url (API.distanceMatrix origins destinations key departureTime mode) "distanceMatrix"
    >>= checkGoogleMapsError url

checkGoogleMapsError :: (MonadThrow m, Log m, HasField "status" a Text) => BaseUrl -> Either ClientError a -> m a
checkGoogleMapsError url res =
  fromEitherM (googleMapsError url) res >>= validateResponseStatus

googleMapsError :: BaseUrl -> ClientError -> ExternalAPICallError
googleMapsError = ExternalAPICallError (Just "GOOGLE_MAPS_API_ERROR")

validateResponseStatus :: (MonadThrow m, Log m, HasField "status" a Text) => a -> m a
validateResponseStatus response =
  case response.status of
    "OK" -> pure response
    "ZERO_RESULTS" -> pure response
    "INVALID_REQUEST" -> throwError GoogleMapsInvalidRequest
    _ -> throwError $ GoogleMapsCallError response.status
