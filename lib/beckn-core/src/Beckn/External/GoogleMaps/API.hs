module Beckn.External.GoogleMaps.API where

import qualified Beckn.External.GoogleMaps.Types as GoogleMaps
import Beckn.Types.App (MandatoryQueryParam)
import Beckn.Utils.Servant.Trail.Client (RequestInfo, withClientTracing)
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

googleMapsAPI :: Proxy GoogleMapsAPI
googleMapsAPI = Proxy

autoComplete :: Text -> Text -> Text -> Integer -> Text -> (RequestInfo, EulerClient GoogleMaps.SearchLocationResp)
placeDetails :: Text -> Text -> Text -> (RequestInfo, EulerClient GoogleMaps.PlaceDetailsResp)
getPlaceName :: Text -> Text -> (RequestInfo, EulerClient GoogleMaps.GetPlaceNameResp)
autoComplete :<|> placeDetails :<|> getPlaceName = client $ withClientTracing googleMapsAPI
