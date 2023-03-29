{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.Maps.MMI.Routes where

import Data.Maybe
import EulerHS.Prelude
import qualified EulerHS.Types as ET
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Utils.Common
import Lib.Error
import qualified Lib.Maps.MMI.MapsClient.Types as MMI
import Servant hiding (throwError)

type MMIRouteAPI =
  "advancedmaps" :> "v1"
    :> Capture "Authorization" Text
    :> "route_eta"
    :> "driving"
    :> Capture "cordinates" Text
    :> QueryParam "steps" Bool
    :> QueryParam "region" Text
    :> QueryParam "overview" Text
    :> Get '[JSON] MMI.RouteResponse

mmiRouteAPI :: Proxy MMIRouteAPI
mmiRouteAPI = Proxy

getRouteClient :: Text -> Text -> Maybe Bool -> Maybe Text -> Maybe Text -> ET.EulerClient MMI.RouteResponse
getRouteClient = ET.client mmiRouteAPI

mmiRoute ::
  ( CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  Text ->
  m MMI.RouteResponse
mmiRoute url apiKey points = do
  callMMIAPI
    url
    (getRouteClient apiKey points (Just True) (Just "ind") (Just "full"))
    "mmi-route"

callMMIAPI :: CallAPI env a
callMMIAPI =
  callApiUnwrappingApiError
    (identity @MMIError)
    Nothing
    (Just "MMI_NOT_AVAILABLE")
