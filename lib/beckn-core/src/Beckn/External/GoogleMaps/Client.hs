module Beckn.External.GoogleMaps.Client where

import qualified Beckn.External.GoogleMaps.API as API
import qualified Beckn.External.GoogleMaps.Types as GoogleMaps
import Beckn.Storage.DB.Config (DBConfig)
import Beckn.Types.App (TraceFlag)
import Beckn.Types.Common
import Beckn.Types.Error
import Beckn.Utils.Common
import Beckn.Utils.Servant.Trail.Client (callAPIWithTrail)
import EulerHS.Prelude
import GHC.Records (HasField)
import Servant.Client.Core (BaseUrl, ClientError)

autoComplete ::
  ( HasField "dbCfg" r DBConfig,
    HasField "traceFlag" r TraceFlag
  ) =>
  BaseUrl ->
  Text ->
  Text ->
  Text ->
  Integer ->
  Text ->
  FlowR r GoogleMaps.SearchLocationResp
autoComplete url apiKey input location radius components = do
  callAPIWithTrail url (API.autoComplete apiKey input location radius components) "autoComplete"
    >>= fromEitherM (googleMapsError url)

placeDetails ::
  ( HasField "dbCfg" r DBConfig,
    HasField "traceFlag" r TraceFlag
  ) =>
  BaseUrl ->
  Text ->
  Text ->
  Text ->
  FlowR r GoogleMaps.PlaceDetailsResp
placeDetails url apiKey placeId fields = do
  callAPIWithTrail url (API.placeDetails apiKey placeId fields) "placeDetails"
    >>= fromEitherM (googleMapsError url)

getPlaceName ::
  ( HasField "dbCfg" r DBConfig,
    HasField "traceFlag" r TraceFlag
  ) =>
  BaseUrl ->
  Text ->
  Text ->
  FlowR r GoogleMaps.GetPlaceNameResp
getPlaceName url latLng apiKey = do
  callAPIWithTrail url (API.getPlaceName latLng apiKey) "getPlaceName"
    >>= fromEitherM (googleMapsError url)

googleMapsError :: BaseUrl -> ClientError -> ExternalAPICallError
googleMapsError = ExternalAPICallErrorWithCode "GOOGLE_MAPS_API_ERROR"
