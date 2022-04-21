module Beckn.External.GoogleMaps.API where

import qualified Beckn.External.GoogleMaps.Types as GoogleMaps
import Beckn.Utils.Common
import EulerHS.Prelude
import EulerHS.Types (EulerClient, client)
import Servant

type GoogleMapsAPI =
  "place" :> "autocomplete" :> "json"
    :> MandatoryQueryParam "key" Text
    :> MandatoryQueryParam "input" Text
    :> MandatoryQueryParam "location" Text
    :> MandatoryQueryParam "radius" Integer
    :> MandatoryQueryParam "components" Text
    :> MandatoryQueryParam "language" Text
    :> Get '[JSON] GoogleMaps.SearchLocationResp
    :<|> "place" :> "details" :> "json"
      :> MandatoryQueryParam "key" Text
      :> MandatoryQueryParam "place_id" Text
      :> MandatoryQueryParam "fields" Text
      :> Get '[JSON] GoogleMaps.PlaceDetailsResp
    :<|> "geocode" :> "json"
      :> MandatoryQueryParam "latlng" Text -- Parameters order is important.
      :> MandatoryQueryParam "key" Text
      :> Get '[JSON] GoogleMaps.GetPlaceNameResp
    :<|> DistanceMatrixAPI
    :<|> DirectionsAPI

type DistanceMatrixAPI =
  "distancematrix" :> "json"
    :> MandatoryQueryParam "origins" [GoogleMaps.Place]
    :> MandatoryQueryParam "destinations" [GoogleMaps.Place]
    :> MandatoryQueryParam "key" Text
    :> QueryParam "departure_time" GoogleMaps.DepartureTime
    :> QueryParam "mode" GoogleMaps.Mode
    :> Post '[JSON] GoogleMaps.DistanceMatrixResp

type DirectionsAPI =
  "directions" :> "json"
    :> MandatoryQueryParam "origin" GoogleMaps.Place
    :> MandatoryQueryParam "destination" GoogleMaps.Place
    :> MandatoryQueryParam "key" Text
    :> QueryParam "alternatives" Bool
    :> QueryParam "mode" GoogleMaps.Mode
    :> QueryParam "waypoints" [GoogleMaps.Place]
    :> Get '[JSON] GoogleMaps.DirectionsResp

googleMapsAPI :: Proxy GoogleMapsAPI
googleMapsAPI = Proxy

autoComplete :: Text -> Text -> Text -> Integer -> Text -> Text -> EulerClient GoogleMaps.SearchLocationResp
placeDetails :: Text -> Text -> Text -> EulerClient GoogleMaps.PlaceDetailsResp
getPlaceName :: Text -> Text -> EulerClient GoogleMaps.GetPlaceNameResp
distanceMatrix ::
  [GoogleMaps.Place] ->
  [GoogleMaps.Place] ->
  Text ->
  Maybe GoogleMaps.DepartureTime ->
  Maybe GoogleMaps.Mode ->
  EulerClient GoogleMaps.DistanceMatrixResp
directions ::
  GoogleMaps.Place ->
  GoogleMaps.Place ->
  Text ->
  Maybe Bool ->
  Maybe GoogleMaps.Mode ->
  Maybe [GoogleMaps.Place] ->
  EulerClient GoogleMaps.DirectionsResp
autoComplete :<|> placeDetails :<|> getPlaceName :<|> distanceMatrix :<|> directions = client googleMapsAPI
