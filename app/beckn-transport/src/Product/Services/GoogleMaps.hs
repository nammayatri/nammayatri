module Product.Services.GoogleMaps where

import App.Types (AppEnv (..), FlowHandler)
import qualified Beckn.External.GoogleMaps.Client as ClientGoogleMaps
import qualified Beckn.External.GoogleMaps.Types as GoogleMaps
import Beckn.Types.Id
import EulerHS.Prelude
import qualified Types.Storage.Person as SP
import Utils.Common (withFlowHandlerAPI)

autoComplete :: Id SP.Person -> Text -> Text -> Integer -> Text -> FlowHandler GoogleMaps.SearchLocationResp
autoComplete _ input location radius lang = withFlowHandlerAPI $ do
  url <- googleMapsUrl <$> ask
  apiKey <- googleMapsKey <$> ask
  let components = "country:in"
  ClientGoogleMaps.autoComplete url apiKey input location radius components lang

placeDetails :: Id SP.Person -> Text -> FlowHandler GoogleMaps.PlaceDetailsResp
placeDetails _ placeId = withFlowHandlerAPI $ do
  url <- googleMapsUrl <$> ask
  apiKey <- googleMapsKey <$> ask
  let fields = "geometry"
  ClientGoogleMaps.placeDetails url apiKey placeId fields

getPlaceName :: Id SP.Person -> Text -> FlowHandler GoogleMaps.GetPlaceNameResp
getPlaceName _ latLng = withFlowHandlerAPI $ do
  url <- googleMapsUrl <$> ask
  apiKey <- googleMapsKey <$> ask
  ClientGoogleMaps.getPlaceName url latLng apiKey
