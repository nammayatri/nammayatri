module Product.Services.GoogleMaps where

import App.Types (AppEnv (..), FlowHandler)
import qualified Beckn.External.GoogleMaps.Client as ClientGoogleMaps
import qualified Beckn.External.GoogleMaps.Types as GoogleMaps
import qualified Beckn.Types.Storage.Person as Person
import EulerHS.Prelude
import Utils.Common (withFlowHandlerAPI)

autoComplete :: Person.Person -> Text -> Text -> Integer -> Text -> FlowHandler GoogleMaps.SearchLocationResp
autoComplete _auth input location radius lang = withFlowHandlerAPI $ do
  url <- googleMapsUrl <$> ask
  apiKey <- googleMapsKey <$> ask
  let components = "country:in"
  ClientGoogleMaps.autoComplete url apiKey input location radius components lang

placeDetails :: Person.Person -> Text -> FlowHandler GoogleMaps.PlaceDetailsResp
placeDetails _auth placeId = withFlowHandlerAPI $ do
  url <- googleMapsUrl <$> ask
  apiKey <- googleMapsKey <$> ask
  let fields = "geometry"
  ClientGoogleMaps.placeDetails url apiKey placeId fields

getPlaceName :: Person.Person -> Text -> FlowHandler GoogleMaps.GetPlaceNameResp
getPlaceName _auth latLng = withFlowHandlerAPI $ do
  url <- googleMapsUrl <$> ask
  apiKey <- googleMapsKey <$> ask
  ClientGoogleMaps.getPlaceName url latLng apiKey
