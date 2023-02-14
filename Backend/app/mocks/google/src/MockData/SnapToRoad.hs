 {-
 Copyright 2022-23, Juspay India Pvt Ltd
 
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License 
 
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program 
 
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
 
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of 
 
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module MockData.SnapToRoad where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as B
import qualified Domain.Types.MockPlace as DPlace
import qualified Domain.Types.MockRoute as DRoute
import Environment
import Kernel.External.Maps.Google.MapsClient
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common

getFolderPath :: MockDataFlow m r => m String
getFolderPath = asks (.mockDataPath) <&> (<> "snap-to-road/")

getRequestsPath :: MockDataFlow m r => m String
getRequestsPath = getFolderPath <&> (<> "requests.json")

getResponsePath :: MockDataFlow m r => DRoute.RouteId -> m String
getResponsePath routeId = getFolderPath <&> (<> "response/" <> show routeId <> ".json")

getAvailableRoutes :: MockDataFlow m r => m [DRoute.MockRoute]
getAvailableRoutes = do
  bs <- liftIO . B.readFile =<< getRequestsPath
  A.eitherDecode bs & fromEitherM (\err -> InternalError $ "Failed to parse available routes: " <> show err)

findResponseByRouteId :: MockDataFlow m r => DRoute.RouteId -> m DPlace.SnapToRoadResponse
findResponseByRouteId routeId = do
  bs <- liftIO . B.readFile =<< getResponsePath routeId
  A.eitherDecode bs & fromEitherM (\err -> InternalError $ "Failed to parse route with id = " <> show routeId <> ": " <> show err)

saveNewResponse :: MockDataFlow m r => [LocationS] -> DPlace.SnapToRoadResponse -> [DRoute.MockRoute] -> m ()
saveNewResponse route response oldRoutes = do
  let routeId = DRoute.RouteId $ length oldRoutes + 1
  let mockRoute = DRoute.MockRoute {routeId, route}
  requestsPath <- getRequestsPath
  responsePath <- getResponsePath routeId
  liftIO $ A.encodeFile requestsPath (oldRoutes <> [mockRoute])
  liftIO $ A.encodeFile responsePath response
