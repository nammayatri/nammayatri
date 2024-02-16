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

handler ::
  Maybe Text ->
  Text ->
  Maybe Maps.LatLong ->
  Maybe Text ->
  Maybe Language ->
  FlowHandler GoogleMaps.GetPlaceNameResp
handler _sessionToken key mbLatLng _placeId _language = withFlowHandlerAPI' $ do
  unless (key == Data.mockKey) $ throwError AccessDenied
  latLng <- mbLatLng & fromMaybeM (NotImplemented "getPlaceName is not implemented: latlng: Nothing")
  pure $ Data.mkMockPlaceNameResp latLng
