{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Product.Services.GoogleMaps where

import App.Types (AppEnv (..), FlowHandler)
import qualified Beckn.External.GoogleMaps.API as API
import qualified Beckn.External.GoogleMaps.Types as GoogleMaps
import qualified Beckn.Types.Storage.RegistrationToken as RegToken
import Beckn.Utils.Common (fromClientError, throwError500, withFlowHandler)
import Beckn.Utils.Servant.Trail.Client (callAPIWithTrail)
import qualified EulerHS.Language as L
import EulerHS.Prelude

autoComplete :: RegToken.RegistrationToken -> Text -> Text -> Integer -> FlowHandler GoogleMaps.SearchLocationResp
autoComplete _auth input location radius = withFlowHandler $ do
  url <- googleMapsUrl <$> ask
  apiKey <- googleMapsKey <$> ask
  res <- callAPIWithTrail url (API.autoComplete apiKey input location radius "country:in") "autoComplete"
  case res of
    Right x -> return x
    Left cliErr -> do
      let err = fromClientError cliErr
      L.logError @Text "client Google maps API autoComplete call error" $ (err ^. #_message) ?: "Some error"
      throwError500 "Google maps API autoComplete error"

placeDetails :: RegToken.RegistrationToken -> Text -> FlowHandler GoogleMaps.PlaceDetailsResp
placeDetails _auth placeId = withFlowHandler $ do
  url <- googleMapsUrl <$> ask
  apiKey <- googleMapsKey <$> ask
  res <- callAPIWithTrail url (API.placeDetails apiKey placeId "geometry") "placeDetails"
  case res of
    Right x -> return x
    Left cliErr -> do
      let err = fromClientError cliErr
      L.logError @Text "client Google maps API placeDetails call error" $ (err ^. #_message) ?: "Some error"
      throwError500 "Google maps API placeDetails error"

getPlaceName :: RegToken.RegistrationToken -> Text -> FlowHandler GoogleMaps.GetPlaceNameResp
getPlaceName _auth latLng = withFlowHandler $ do
  url <- googleMapsUrl <$> ask
  apiKey <- googleMapsKey <$> ask
  res <- callAPIWithTrail url (API.getPlaceName latLng apiKey) "getPlaceName"
  case res of
    Right x -> return x
    Left cliErr -> do
      let err = fromClientError cliErr
      L.logError @Text "client Google maps API getPlaceName call error" $ (err ^. #_message) ?: "Some error"
      throwError500 "Google maps API getPlaceName error"
