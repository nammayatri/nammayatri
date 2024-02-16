{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.SnapToRoad
  ( handler,
  )
where

import qualified Domain.Types.MockPlace as DPlace
import qualified Domain.Types.MockRoute as DRoute
import Environment
import Kernel.External.Maps.Google.MapsClient.Types as GoogleMaps
import Kernel.Prelude
import Kernel.Utils.Common
import qualified MockData.Common as Data
import qualified MockData.SnapToRoad as QSnapToRoad
import Servant.API (FromHttpApiData (..))
import qualified Tools.Client as Client
import Tools.Error

handler :: Text -> Bool -> Text -> FlowHandler DPlace.SnapToRoadResponse
handler key interpolate path = withFlowHandlerAPI' $ do
  unless (key == Data.mockKey) $ throwError AccessDenied
  unless interpolate $
    throwError $ NotImplemented $ "snapToRoad is not implemented: interpolate: " <> show interpolate
  places <- either (throwError . InvalidRequest) pure $ parseRoute path
  route <-
    mkRoute places
      & fromMaybeM (NotImplemented "snapToRoad is not implemented: path should only contain coordinates, not addresses")
  snapToRoadIdentityMode <- asks (.snapToRoadIdentityMode)
  if snapToRoadIdentityMode
    then do
      logInfo "Using identical snap to road response"
      pure $ QSnapToRoad.mkIdenticalResponse route
    else do
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
