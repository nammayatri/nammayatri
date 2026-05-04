{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

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
import qualified Tools.PlaceCache as PlaceCache

handler ::
  Maybe Text ->
  Text ->
  Maybe Maps.LatLong ->
  Maybe Text ->
  Maybe Language ->
  FlowHandler GoogleMaps.GetPlaceNameResp
handler _sessionToken key mbLatLng mbPlaceId _language = withFlowHandlerAPI' $ do
  unless (key == Data.mockKey) $ throwError AccessDenied
  -- Resolve in priority order:
  --   1. ?place_id=<id> looked up in the in-mem cache populated by the
  --      autocomplete handler. Falls back to decoding lat/lng embedded
  --      in the placeId string itself (mock:<lat>:<lon>) so the
  --      "user picks a suggestion" flow stays consistent across
  --      mock-google restarts.
  --   2. ?latlng=<lat>,<lon> — explicit coordinate.
  --   3. neither: 501 like the legacy mock.
  resolved <- case mbPlaceId of
    Just pid -> do
      mbCached <- liftIO $ PlaceCache.lookupPlace pid
      case mbCached of
        Just (la, lo) -> pure $ Maps.LatLong {lat = la, lon = lo}
        Nothing ->
          mbLatLng
            & fromMaybeM
              ( NotImplemented $
                  "getPlaceName is not implemented: unknown placeId " <> pid
              )
    Nothing ->
      mbLatLng
        & fromMaybeM
          (NotImplemented "getPlaceName is not implemented: latlng + place_id both Nothing")
  pure $ Data.mkMockPlaceNameResp resolved
