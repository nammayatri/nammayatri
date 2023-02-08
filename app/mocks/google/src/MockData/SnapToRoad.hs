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
