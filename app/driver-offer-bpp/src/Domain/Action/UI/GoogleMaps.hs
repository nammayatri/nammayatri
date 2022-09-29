module Domain.Action.UI.GoogleMaps
  ( GoogleMaps.GetPlaceNameResp (..),
    getPlaceName,
  )
where

import qualified Beckn.External.GoogleMaps.Client as ClientGoogleMaps
import qualified Beckn.External.GoogleMaps.Types as GoogleMaps
import Beckn.Utils.Common (MonadFlow)
import EulerHS.Prelude
import Tools.Metrics

getPlaceName ::
  (MonadFlow m, GoogleMaps.HasGoogleMaps m r, CoreMetrics m) =>
  Text ->
  Maybe GoogleMaps.Language ->
  m GoogleMaps.GetPlaceNameResp
getPlaceName latLng language = do
  url <- asks (.googleMapsUrl)
  apiKey <- asks (.googleMapsKey)
  ClientGoogleMaps.getPlaceName url latLng apiKey language
