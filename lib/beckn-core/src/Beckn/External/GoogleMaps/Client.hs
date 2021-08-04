module Beckn.External.GoogleMaps.Client where

import qualified Beckn.External.GoogleMaps.API as API
import qualified Beckn.External.GoogleMaps.Types as GoogleMaps
import Beckn.Storage.DB.Config (DBConfig)
import Beckn.Types.Common
import Beckn.Types.Error
import Beckn.Types.Monitoring.Prometheus.Metrics (HasCoreMetrics)
import Beckn.Utils.Common
import EulerHS.Prelude
import GHC.Records.Extra (HasField)
import Servant.Client.Core (ClientError)

autoComplete ::
  ( HasField "dbCfg" r DBConfig,
    HasCoreMetrics r
  ) =>
  BaseUrl ->
  Text ->
  Text ->
  Text ->
  Integer ->
  Text ->
  Text ->
  FlowR r GoogleMaps.SearchLocationResp
autoComplete url apiKey input location radius components lang = do
  callAPI url (API.autoComplete apiKey input location radius components lang) "autoComplete"
    >>= checkGoogleMapsError url

placeDetails ::
  ( HasField "dbCfg" r DBConfig,
    HasCoreMetrics r
  ) =>
  BaseUrl ->
  Text ->
  Text ->
  Text ->
  FlowR r GoogleMaps.PlaceDetailsResp
placeDetails url apiKey placeId fields = do
  callAPI url (API.placeDetails apiKey placeId fields) "placeDetails"
    >>= checkGoogleMapsError url

getPlaceName ::
  ( HasField "dbCfg" r DBConfig,
    HasCoreMetrics r
  ) =>
  BaseUrl ->
  Text ->
  Text ->
  FlowR r GoogleMaps.GetPlaceNameResp
getPlaceName url latLng apiKey = do
  callAPI url (API.getPlaceName latLng apiKey) "getPlaceName"
    >>= checkGoogleMapsError url

checkGoogleMapsError :: (HasField "status" a Text) => BaseUrl -> Either ClientError a -> FlowR r a
checkGoogleMapsError url res =
  fromEitherM (googleMapsError url) res >>= validateResponseStatus

googleMapsError :: BaseUrl -> ClientError -> ExternalAPICallError
googleMapsError = ExternalAPICallError (Just "GOOGLE_MAPS_API_ERROR")

validateResponseStatus :: (HasField "status" a Text) => a -> FlowR r a
validateResponseStatus response =
  case response.status of
    "OK" -> pure response
    "ZERO_RESULTS" -> pure response
    "INVALID_REQUEST" -> throwError GoogleMapsInvalidRequest
    _ -> throwError $ GoogleMapsCallError response.status
