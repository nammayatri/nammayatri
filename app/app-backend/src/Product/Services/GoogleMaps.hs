module Product.Services.GoogleMaps where

import App.Types (AppEnv (..), FlowHandler)
import qualified Beckn.External.GoogleMaps.Client as ClientGoogleMaps
import qualified Beckn.External.GoogleMaps.Types as GoogleMaps
import Beckn.Types.Id
import Beckn.Utils.Logging
import qualified Domain.Types.Person as Person
import EulerHS.Prelude
import Utils.Common (withFlowHandlerAPI)

autoComplete :: Id Person.Person -> Text -> Text -> Integer -> Text -> FlowHandler GoogleMaps.SearchLocationResp
autoComplete personId input location radius lang = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  url <- googleMapsUrl <$> ask
  apiKey <- googleMapsKey <$> ask
  let components = "country:in"
  ClientGoogleMaps.autoComplete url apiKey input location radius components lang

placeDetails :: Id Person.Person -> Text -> FlowHandler GoogleMaps.PlaceDetailsResp
placeDetails personId placeId = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  url <- googleMapsUrl <$> ask
  apiKey <- googleMapsKey <$> ask
  let fields = "geometry"
  ClientGoogleMaps.placeDetails url apiKey placeId fields

getPlaceName :: Id Person.Person -> Text -> FlowHandler GoogleMaps.GetPlaceNameResp
getPlaceName personId latLng = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  url <- googleMapsUrl <$> ask
  apiKey <- googleMapsKey <$> ask
  ClientGoogleMaps.getPlaceName url latLng apiKey
