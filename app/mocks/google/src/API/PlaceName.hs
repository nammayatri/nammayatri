module API.PlaceName
  ( handler,
  )
where

import Environment
import Kernel.External.Maps.Google.MapsClient.Types as GoogleMaps
import qualified Kernel.External.Maps.Types as Maps
import Kernel.External.Types (Language)
import Kernel.Prelude
import Kernel.Utils.Common
import qualified MockData.Common as Data
import qualified MockData.PlaceName as Data
import Tools.Error

handler ::
  Maybe Text ->
  Text ->
  Maybe Maps.LatLong ->
  Maybe Text ->
  Maybe Language ->
  FlowHandler GoogleMaps.GetPlaceNameResp
handler _sessionToken key mbLatLng _placeId _language = withFlowHandlerAPI $ do
  unless (key == Data.mockKey) $ throwError AccessDenied
  latLng <- mbLatLng & fromMaybeM (NotImplemented "getPlaceName is not implemented: latlng: Nothing")
  pure $ Data.mkMockPlaceNameResp latLng
