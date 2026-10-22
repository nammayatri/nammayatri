{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.RideEstimatedRoute
  ( getRideEstimatedRoute,
  )
where

import qualified "this" API.Types.ProviderPlatform.Management.Ride as Common
import Data.Aeson (eitherDecodeStrict)
import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Domain.Types.Ride as DRide
import Environment
import Kernel.Beam.Functions (runInReplica)
import qualified Kernel.External.Maps.Google.MapsClient.Types as Google
import qualified Kernel.External.Maps.Google.PolyLinePoints as PolyLinePoints
import qualified Kernel.External.Maps.OSRM.RoadsClient as OSRM
import Kernel.Prelude
import qualified Kernel.Storage.ClickhouseV2 as CH
import Kernel.Utils.Common
import qualified Storage.Clickhouse.ExternalApiCallLogs as CHExternalApiCallLogs
import qualified Storage.Queries.Booking as QBooking
import Tools.Error

routeApiNames :: [Text]
routeApiNames = ["advancedDirectionsAPI", "directions", "OsrmRouteAPI"]

searchLookback :: NominalDiffTime
searchLookback = 60 * 60

searchLookahead :: NominalDiffTime
searchLookahead = 10 * 60

maxCalls :: Int
maxCalls = 20

getRideEstimatedRoute :: DRide.Ride -> Flow Common.RideEstimatedRouteRes
getRideEstimatedRoute ride = do
  booking <- runInReplica (QBooking.findById ride.bookingId) >>= fromMaybeM (BookingDoesNotExist ride.bookingId.getId)
  rows <-
    CHExternalApiCallLogs.findAllByEntityIdAndApiNames
      booking.transactionId
      routeApiNames
      (addUTCTime (negate searchLookback) booking.createdAt)
      (addUTCTime searchLookahead booking.createdAt)
      maxCalls
  -- ReplacingMergeTree, read without FINAL: a row can appear once per unmerged part.
  let uniqueRows = List.nubBy (\(idA, _, _, _, _) (idB, _, _, _, _) -> idA == idB) rows
  pure
    Common.RideEstimatedRouteRes
      { transactionId = booking.transactionId,
        estimatedDistance = booking.estimatedDistance,
        calls = mapMaybe toCall uniqueRows
      }

toCall :: (Text, Maybe Text, Maybe Text, CH.DateTime, Maybe Text) -> Maybe Common.EstimatedRouteCall
toCall (_, mbApiName, mbProvider, CH.DateTime calledAt, mbPayload) = do
  apiName <- mbApiName
  payload <- mbPayload
  let routes = filter ((> 1) . length . (.points)) $ parseRoutes apiName payload
  guard (not $ null routes)
  pure Common.EstimatedRouteCall {apiName, provider = fromMaybe "" mbProvider, calledAt, routes}

parseRoutes :: Text -> Text -> [Common.EstimatedRouteOption]
parseRoutes apiName payload = case apiName of
  "advancedDirectionsAPI" -> foldMap (map fromRouteV2 . (.routes)) (decodePayload @Google.AdvancedDirectionsResp)
  "directions" -> foldMap (map fromDirectionsRoute . (.routes)) (decodePayload @Google.DirectionsResp)
  "OsrmRouteAPI" -> foldMap (map fromOsrmRoute . (.routes)) (decodePayload @OSRM.OSRMRouteResponse)
  _ -> []
  where
    decodePayload :: forall a. FromJSON a => Maybe a
    decodePayload = rightToMaybe . eitherDecodeStrict $ TE.encodeUtf8 payload

fromRouteV2 :: Google.RouteV2 -> Common.EstimatedRouteOption
fromRouteV2 route =
  Common.EstimatedRouteOption
    { distanceMeters = Just route.distanceMeters,
      durationSec = parseProtoDuration route.duration,
      points = case route.polyline of
        Just polyline | not (T.null polyline.encodedPolyline) -> PolyLinePoints.decode polyline.encodedPolyline
        _ -> concatMap (\leg -> concatMap (\step -> PolyLinePoints.decode step.polyline.encodedPolyline) leg.steps) route.legs
    }

fromDirectionsRoute :: Google.Route -> Common.EstimatedRouteOption
fromDirectionsRoute route =
  Common.EstimatedRouteOption
    { distanceMeters = Just . sum $ map (\leg -> leg.distance.value) route.legs,
      durationSec = Just . sum $ map (\leg -> leg.duration.value) route.legs,
      points = concatMap (\leg -> concatMap (\step -> PolyLinePoints.decode step.polyline.points) leg.steps) route.legs
    }

fromOsrmRoute :: OSRM.OSRMRouteRoutes -> Common.EstimatedRouteOption
fromOsrmRoute route =
  Common.EstimatedRouteOption
    { distanceMeters = Just $ round route.distance,
      durationSec = Just $ round route.duration,
      points = map (.getLatLong) route.geometry.coordinates
    }

parseProtoDuration :: Text -> Maybe Int
parseProtoDuration duration = round <$> (readMaybe @Double . T.unpack =<< T.stripSuffix "s" duration)
