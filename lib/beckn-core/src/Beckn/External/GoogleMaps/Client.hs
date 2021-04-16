module Beckn.External.GoogleMaps.Client where

import qualified Beckn.External.GoogleMaps.API as API
import qualified Beckn.External.GoogleMaps.Types as GoogleMaps
import Beckn.Storage.DB.Config (DBConfig)
import Beckn.Types.Common
import Beckn.Types.Error
import Beckn.Utils.Common
import Beckn.Utils.Monitoring.Prometheus.Metrics (HasCoreMetrics)
import qualified EulerHS.Language as L
import EulerHS.Prelude
import GHC.Records (HasField)
import Servant.Client.Core (BaseUrl, ClientError)

autoComplete ::
  ( HasField "dbCfg" r DBConfig,
    HasCoreMetrics (FlowR r)
  ) =>
  BaseUrl ->
  Text ->
  Text ->
  Text ->
  Integer ->
  Text ->
  FlowR r GoogleMaps.SearchLocationResp
autoComplete url apiKey input location radius components = do
  L.callAPI url (API.autoComplete apiKey input location radius components)
    >>= fromEitherM (googleMapsError url)

placeDetails ::
  ( HasField "dbCfg" r DBConfig,
    HasCoreMetrics (FlowR r)
  ) =>
  BaseUrl ->
  Text ->
  Text ->
  Text ->
  FlowR r GoogleMaps.PlaceDetailsResp
placeDetails url apiKey placeId fields = do
  L.callAPI url (API.placeDetails apiKey placeId fields)
    >>= fromEitherM (googleMapsError url)

getPlaceName ::
  ( HasField "dbCfg" r DBConfig,
    HasCoreMetrics (FlowR r)
  ) =>
  BaseUrl ->
  Text ->
  Text ->
  FlowR r GoogleMaps.GetPlaceNameResp
getPlaceName url latLng apiKey = do
  L.callAPI url (API.getPlaceName latLng apiKey)
    >>= fromEitherM (googleMapsError url)

googleMapsError :: BaseUrl -> ClientError -> ExternalAPICallError
googleMapsError = ExternalAPICallErrorWithCode "GOOGLE_MAPS_API_ERROR"
