module MockData.SnapToRoad where

import Beckn.External.Maps.Google.MapsClient
import Beckn.Prelude
import Beckn.Types.Common
import Beckn.Types.Error
import Beckn.Utils.Common
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as B
import qualified Domain.Types.MockPlace as DPlace
import qualified Domain.Types.MockRoute as DRoute

folderPath :: String
folderPath = "./app/mocks/google/mock-data/snap-to-road/"

requestsPath :: String
requestsPath = folderPath <> "requests.json"

responsePath :: DRoute.RouteId -> String
responsePath routeId = folderPath <> "response/" <> show routeId <> ".json"

getAvailableRoutes :: (MonadIO m, Log m, MonadThrow m) => m [DRoute.MockRoute]
getAvailableRoutes = do
  bs <- liftIO . B.readFile $ requestsPath
  A.eitherDecode bs & fromEitherM (\err -> InternalError $ "Failed to parse available routes: " <> show err)

findResponseByRouteId :: (MonadThrow m, MonadIO m, Log m) => DRoute.RouteId -> m DPlace.SnapToRoadResponse
findResponseByRouteId routeId = do
  bs <- liftIO . B.readFile $ responsePath routeId
  A.eitherDecode bs & fromEitherM (\err -> InternalError $ "Failed to parse route with id = " <> show routeId <> ": " <> show err)

saveNewResponse :: MonadIO m => [LocationS] -> DPlace.SnapToRoadResponse -> [DRoute.MockRoute] -> m ()
saveNewResponse route response oldRoutes = do
  let routeId = DRoute.RouteId $ length oldRoutes + 1
  let mockRoute = DRoute.MockRoute {routeId, route}
  liftIO $ A.encodeFile requestsPath (oldRoutes <> [mockRoute])
  liftIO $ A.encodeFile (responsePath routeId) response
