module Domain.Action.UI.GoogleMaps
  ( GoogleMaps.GetPlaceNameResp (..),
    getPlaceName,
  )
where

import qualified Beckn.External.GoogleMaps.Client as ClientGoogleMaps
import qualified Beckn.External.GoogleMaps.Types as GoogleMaps
import EulerHS.Prelude
import Tools.Metrics
import Utils.Common (MonadFlow)

getPlaceName :: (MonadFlow m, GoogleMaps.HasGoogleMaps m r, CoreMetrics m) => Text -> m GoogleMaps.GetPlaceNameResp
getPlaceName latLng = do
  url <- asks (.googleMapsUrl)
  apiKey <- asks (.googleMapsKey)
  ClientGoogleMaps.getPlaceName url latLng apiKey
