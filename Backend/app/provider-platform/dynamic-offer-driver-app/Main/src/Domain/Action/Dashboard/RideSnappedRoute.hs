{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.RideSnappedRoute
  ( getRideSnappedRoute,
  )
where

import qualified "this" API.Types.ProviderPlatform.Management.Ride as Common
import qualified Domain.Types.Ride as DRide
import Environment
import qualified Kernel.External.Maps.Google.RoadsClient as GoogleRoads
import qualified Kernel.External.Maps.OSRM.RoadsClient as OSRM
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.Prelude
import qualified Kernel.Storage.ClickhouseV2 as CH
import Kernel.Utils.CalculateDistance (getRouteLinearLength)
import Kernel.Utils.Common
import Storage.Clickhouse.ExternalApiCallLogs (decodePayload)
import qualified Storage.Clickhouse.ExternalApiCallLogs as CHExternalApiCallLogs

snapApiNames :: [Text]
snapApiNames = ["snapToRoad", "OsrmMatchAPI"]

endLookahead :: NominalDiffTime
endLookahead = 60 * 60

maxCalls :: Int
maxCalls = 500

newtype SnapRequestPayload = SnapRequestPayload {points :: [LatLong]}
  deriving (Generic, FromJSON)

getRideSnappedRoute :: DRide.Ride -> Flow Common.RideSnappedRouteRes
getRideSnappedRoute ride = do
  now <- getCurrentTime
  rows <-
    CHExternalApiCallLogs.findAllByEntityIdAndApiNames
      ride.id.getId
      snapApiNames
      ride.createdAt
      (addUTCTime endLookahead (fromMaybe now ride.tripEndTime))
      maxCalls
  pure Common.RideSnappedRouteRes {calls = mapMaybe toCall rows}

toCall :: CHExternalApiCallLogs.ExternalApiCallLogs -> Maybe Common.SnapToRoadCall
toCall row = do
  apiName <- row.apiName
  let inputPoints = maybe [] (.points) (decodePayload @SnapRequestPayload =<< row.requestPayload)
      -- A failed call logs the error body instead, which leaves no snapped points.
      (snappedPoints, mbDistance) = maybe ([], Nothing) (parseSnapped apiName) row.responsePayload
      CH.DateTime calledAt = row.createdAt
  pure
    Common.SnapToRoadCall
      { apiName,
        provider = fromMaybe "" row.svcProvider,
        calledAt,
        distanceMeters = mbDistance,
        inputPoints,
        snappedPoints
      }

parseSnapped :: Text -> Text -> ([LatLong], Maybe Int)
parseSnapped apiName payload = case apiName of
  "snapToRoad" ->
    let points =
          maybe [] (map (\sp -> LatLong sp.location.latitude sp.location.longitude) . (.snappedPoints)) $
            decodePayload @(GoogleRoads.SnapToRoadResponse' GoogleRoads.SPLocation) payload
     in (points, if length points > 1 then Just . (.getMeters) . highPrecMetersToMeters $ getRouteLinearLength points Nothing else Nothing)
  "OsrmMatchAPI" -> case decodePayload @OSRM.MatchResp payload of
    Just resp ->
      let matchings = toList resp.matchings
       in (concatMap (\m -> map (.getLatLong) m.geometry.coordinates) matchings, Just . round . sum $ map (.distance) matchings)
    Nothing -> ([], Nothing)
  _ -> ([], Nothing)
