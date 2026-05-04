{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Directions
  ( handler,
  )
where

import Environment
import Kernel.External.Maps.Google.MapsClient.Types as GoogleMaps
import Kernel.Prelude
import Kernel.Utils.Common
import qualified MockData.Common as Data
import qualified MockData.Directions as Data
import Tools.Error
import qualified Tools.OSRM as OSRM

handler ::
  GoogleMaps.Place ->
  GoogleMaps.Place ->
  Text ->
  Maybe Bool ->
  Maybe GoogleMaps.Mode ->
  Maybe [GoogleMaps.Place] ->
  Maybe Text ->
  FlowHandler GoogleMaps.DirectionsResp
handler origin destination key _alternatives mode waypoints avoid = withFlowHandlerAPI' $ do
  unless (key == Data.mockKey) $ throwError AccessDenied
  unless (isNothing mode || mode == Just GoogleMaps.DRIVING) $
    throwError $ NotImplemented $ "directions API is not implemented: mode: " <> show mode
  unless (isEmptyWaypoint waypoints) $
    throwError $ NotImplemented $ "directions API is not implemented: waypoints: " <> show waypoints
  unless (isNothing avoid || avoid == Just "tolls" || avoid == Just "tolls|ferries") $
    throwError $ NotImplemented $ "directions API is not implemented: avoid: " <> show avoid
  case (origin, destination) of
    (GoogleMaps.Location origin', GoogleMaps.Location destination') -> do
      -- Try OSRM first for real road distance/duration/polyline.
      -- Fall back to the straight-line mock when osrm-routed is down.
      mbOsrm <- liftIO $ OSRM.getRoute (origin'.lat, origin'.lng) (destination'.lat, destination'.lng)
      pure $ case mbOsrm of
        Just r -> Data.mkDirectionsRespOsrm origin' destination' r
        Nothing -> Data.mkDirectionsResp origin' destination'
    _ -> throwError $ NotImplemented "Both origin and destination should contain coordinates"
  where
    isEmptyWaypoint Nothing = True
    isEmptyWaypoint (Just [GoogleMaps.Address ""]) = True
    isEmptyWaypoint _ = True -- TODO: Handle it later
