{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.CallSpecialZone where

import API.Types
import Data.Maybe
import Data.Proxy
import Domain.Types.SpecialZone (SpecialZone)
import EulerHS.Types (EulerClient, client)
import Kernel.External.Maps (LatLong)
import Kernel.External.Slack.Types
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.App
import Kernel.Utils.Servant.Client

type SzLookupAPI = SpecialZoneAPI

szLookupClient :: LatLong -> EulerClient (Maybe SpecialZone)
szLookupClient = client szLookupApi

szLookupApi :: Proxy SzLookupAPI
szLookupApi = Proxy

szLookup ::
  (MonadFlow m, CoreMetrics m) =>
  BaseUrl ->
  LatLong ->
  m (Maybe SpecialZone)
szLookup baseUrl latLong = do
  callApiUnwrappingApiError (identity @Error) Nothing (Just "SPECIAL_ZONE_API_ERROR") baseUrl (szLookupClient latLong) "SpecialZoneLookup" szLookupApi
