module API.SnapToRoad
  ( handler,
  )
where

import Beckn.External.Maps.Google.MapsClient.Types as GoogleMaps
import Beckn.Prelude
import Beckn.Utils.Common
import qualified Domain.Types.MockPlace as DPlace
import qualified Domain.Types.MockRoute as DRoute
import Environment
import qualified MockData.Common as Data
import qualified MockData.SnapToRoad as QSnapToRoad
import Servant.API (FromHttpApiData (..))
import qualified Tools.Client as Client
import Tools.Error

handler :: Text -> Bool -> Text -> FlowHandler DPlace.SnapToRoadResponse
handler key interpolate path = withFlowHandlerAPI $ do
  unless (key == Data.mockKey) $ throwError AccessDenied
  unless interpolate $
    throwError $ NotImplemented $ "snapToRoad is not implemented: interpolate: " <> show interpolate
  places <- either (throwError . InvalidRequest) pure $ parseRoute path
  route <-
    mkRoute places
      & fromMaybeM (NotImplemented "snapToRoad is not implemented: path should only contain coordinates, not addresses")

  availableRoutes <- QSnapToRoad.getAvailableRoutes
  let mbSavedRouteId = DRoute.lookupRoute route availableRoutes
  case mbSavedRouteId of
    Nothing -> do
      googleCfg <-
        asks (.googleCfg)
          >>= fromMaybeM (NotImplemented $ "snapToRoad is not implemented: path: " <> show route)
      logWarning "Using real google for caching snapToRoad results"
      snapToRoadResponse <- Client.snapToRoad googleCfg.googleRoadsUrl googleCfg.googleKey path
      QSnapToRoad.saveNewResponse route snapToRoadResponse availableRoutes
      pure snapToRoadResponse
    Just savedRouteId -> do
      logInfo "Using cached snapToRoad result from file"
      QSnapToRoad.findResponseByRouteId savedRouteId

parseRoute :: Text -> Either Text [GoogleMaps.Place]
parseRoute = parseUrlPiece

mkRoute :: [Place] -> Maybe [LocationS]
mkRoute = foldl func (Just [])
  where
    func Nothing _ = Nothing
    func _ (Address _) = Nothing
    func (Just locations) (Location place) = Just $ place : locations
