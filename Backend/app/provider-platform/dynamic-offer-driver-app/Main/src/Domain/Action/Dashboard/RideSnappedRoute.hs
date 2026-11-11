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
import Data.Either (isRight)
import Data.List (elemIndex, groupBy, partition, sortOn)
import qualified Data.List.NonEmpty as NE
import qualified Domain.Action.Dashboard.RideEstimatedRoute as RideEstimatedRoute
import qualified Domain.Types.Ride as DRide
import Environment
import qualified Kernel.External.Maps as Maps
import qualified Kernel.External.Maps.Google.RoadsClient as GoogleRoads
import qualified Kernel.External.Maps.OSRM.RoadsClient as OSRM
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.Prelude
import qualified Kernel.Storage.ClickhouseV2 as CH
import Kernel.Types.Error
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters, getRouteLinearLength)
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import Lib.LocationUpdates (approxRouteEntityIdSuffix, pickupSnapEntityIdSuffix, snapToRoadReqPoints)
import Storage.Clickhouse.ExternalApiCallLogs (decodePayload)
import qualified Storage.Clickhouse.ExternalApiCallLogs as CHExternalApiCallLogs
import Storage.ConfigPilot.Config.MerchantServiceUsageConfig (MerchantServiceUsageConfigDimensions (..))
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))

googleSnapApiName, osrmSnapApiName :: Text
googleSnapApiName = "snapToRoad"
osrmSnapApiName = "OsrmMatchAPI"

endLookahead :: NominalDiffTime
endLookahead = 60 * 60

maxCalls :: Int
maxCalls = 500

rectificationWindow :: NominalDiffTime
rectificationWindow = 2 * 60

data SnapRequestPayload = SnapRequestPayload
  { points :: [LatLong],
    calculateDistanceFrom :: Maybe LatLong
  }
  deriving (Generic, FromJSON)

data SnapCall = SnapCall
  { provider :: Maps.MapsService,
    calledAt :: UTCTime,
    request :: Maps.SnapToRoadReq,
    response :: Either Common.SnapRejectReason Maps.SnapToRoadResp
  }

data Batch = Batch
  { startedAt :: UTCTime,
    attempts :: [SnapCall],
    sections :: [SnapCall]
  }

getRideSnappedRoute :: DRide.Ride -> Flow Common.RideSnappedRouteRes
getRideSnappedRoute ride = do
  now <- getCurrentTime
  let pickupEntityId = ride.id.getId <> pickupSnapEntityIdSuffix
      approxRouteEntityId = ride.id.getId <> approxRouteEntityIdSuffix
      rideDistanceFailed = ride.distanceCalculationFailed == Just True
  rows <-
    CHExternalApiCallLogs.findAllByEntityIdAndApiNames
      [ride.id.getId, pickupEntityId, approxRouteEntityId]
      ([googleSnapApiName, osrmSnapApiName] <> RideEstimatedRoute.routeApiNames)
      ride.createdAt
      (addUTCTime endLookahead (fromMaybe now ride.tripEndTime))
      maxCalls
  serviceUsageConfig <- getOneConfig (MerchantServiceUsageConfigDimensions {merchantOperatingCityId = ride.merchantOperatingCityId.getId}) Nothing >>= fromMaybeM (MerchantServiceUsageConfigNotFound ride.merchantOperatingCityId.getId)
  mbConfidenceThreshold <-
    if rideDistanceFailed
      then do
        transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = ride.merchantOperatingCityId.getId}) Nothing >>= fromMaybeM (TransporterConfigNotFound ride.merchantOperatingCityId.getId)
        pure $ if transporterConfig.useWithSnapToRoadFallback then Just transporterConfig.snapToRoadConfidenceThreshold else Nothing
      else pure Nothing
  let snapRows = filter (\row -> row.entityId /= Just approxRouteEntityId && row.apiName `elem` [Just googleSnapApiName, Just osrmSnapApiName]) rows
      (pickupRows, rideRows) = partition (\row -> row.entityId == Just pickupEntityId) snapRows
      approxRoute = do
        call <- lastMay $ mapMaybe RideEstimatedRoute.toCall (filter (\row -> row.entityId == Just approxRouteEntityId) rows)
        route <- listToMaybe $ sortOn (.durationSec) (reverse call.routes)
        pure Common.MapsRouteCall {apiName = call.apiName, provider = call.provider, calledAt = call.calledAt, routes = [route]}
  (pickupBatches, pickupFailure) <- legBatches [Maps.OSRM] Nothing Common.ON_PICKUP (ride.pickupDistanceCalculationFailed == Just True) (mapMaybe toSnapCall pickupRows)
  (rideBatches, rideFailure) <- legBatches serviceUsageConfig.snapToRoadProvidersList mbConfidenceThreshold Common.ON_RIDE rideDistanceFailed (mapMaybe toSnapCall rideRows)
  pure
    Common.RideSnappedRouteRes
      { batches = pickupBatches <> rideBatches,
        distanceFailures = catMaybes [pickupFailure, rideFailure],
        approxRoute
      }

legBatches :: [Maps.MapsService] -> Maybe Double -> Common.Status -> Bool -> [SnapCall] -> Flow ([Common.SnapBatch], Maybe Common.SnapDistanceFailure)
legBatches providers mbConfidenceThreshold rideStatus distanceFailed calls = do
  droppedPointsThreshold <- asks (.droppedPointsThreshold)
  let batches =
        reverse $
          foldl'
            ( \acc call -> case acc of
                batch : rest | Just batch' <- joinBatch droppedPointsThreshold batch call -> batch' : rest
                _ -> Batch {startedAt = call.calledAt, attempts = [call], sections = []} : acc
            )
            []
            (orderCalls providers calls)
      toSnapBatch batch snapped = Common.SnapBatch {rideStatus, calledAt = batch.startedAt, inputPoints = batchInput batch, snapped}
      untilFailure = \case
        [] -> pure ([], Nothing)
        batch : rest -> do
          rejections <- batchRejections mbConfidenceThreshold batch
          if null rejections
            then (\(rest', failure) -> (batch : rest', failure)) <$> untilFailure rest
            else pure ([], Just (batch, rejections))
  (used, mbFailed) <- if distanceFailed then untilFailure batches else pure (batches, Nothing)
  pure
    ( map (\batch -> toSnapBatch batch (batchResult batch)) used <> maybe [] (\(batch, _) -> [toSnapBatch batch Nothing]) mbFailed,
      mbFailed <&> \(batch, reasons) -> Common.SnapDistanceFailure {rideStatus, calledAt = batch.startedAt, attempts = reasons}
    )

orderCalls :: [Maps.MapsService] -> [SnapCall] -> [SnapCall]
orderCalls providers calls = concat (go Nothing groups)
  where
    groups = map (sortOn (\call -> (call.calledAt, providerRank call))) . groupBy ((==) `on` (.request.points)) $ sortOn (.request.points) calls
    providerRank call = fromMaybe (length providers) (elemIndex call.provider providers)
    groupPoints group = maybe [] (.request.points) (listToMaybe group)
    groupTime group = minimum (map (.calledAt) group)
    go _ [] = []
    go mbEnd remaining =
      let earliest = minimum (map groupTime remaining)
          candidates = filter ((== earliest) . groupTime) remaining
          ranked = case mbEnd of
            Just end -> sortOn (\group -> maybe (1 :: Int, 0) ((0,) . distanceBetweenInMeters end) (listToMaybe (groupPoints group))) candidates
            Nothing -> sortOn (\group -> listToMaybe (groupPoints group) `elem` map (lastMay . groupPoints) remaining) candidates
       in case ranked of
            next : _ -> next : go (lastMay (groupPoints next)) (filter ((/= groupPoints next) . groupPoints) remaining)
            [] -> []

batchInput :: Batch -> [LatLong]
batchInput batch = case batch.attempts of
  attempt : _ -> attempt.request.points
  [] -> concatMap (.request.points) batch.sections

joinBatch :: HighPrecMeters -> Batch -> SnapCall -> Maybe Batch
joinBatch droppedPointsThreshold batch call
  | diffUTCTime call.calledAt batch.startedAt > rectificationWindow = Nothing
  | call.request.points == batchInput batch && all (\section -> section.request.points == batchInput batch) batch.sections =
    Just $
      if call.provider `notElem` map (.provider) batch.attempts
        then batch {attempts = batch.attempts <> [call]}
        else batch {sections = if any (isRight . (.response)) batch.sections then batch.sections else [call]}
  | not (null batch.sections) && listToMaybe call.request.points == listToMaybe (batchInput batch) =
    Just batch {sections = [call]}
  | continuesAfterGap =
    Just $
      if null batch.sections
        then batch {attempts = [], sections = maybeToList (lastMay batch.attempts) <> [call]}
        else batch {sections = batch.sections <> [call]}
  | otherwise = Nothing
  where
    continuesAfterGap =
      call.request.points /= batchInput batch
        && toInteger (length (batchInput batch) + length call.request.points) <= snapToRoadReqPoints + 1
        && case (listToMaybe (reverse (batchInput batch)), listToMaybe call.request.points) of
          (Just lastPoint, Just firstPoint) -> distanceBetweenInMeters lastPoint firstPoint >= droppedPointsThreshold
          _ -> False

batchResult :: Batch -> Maybe Common.SnappedResult
batchResult batch
  | not (null batch.sections) = if all (isRight . (.response)) batch.sections then snappedResult (show Maps.SelfTuned) batch.sections else Nothing
  | otherwise = case lastMay batch.attempts of
    Just call | isRight call.response -> snappedResult (show call.provider) [call]
    _ -> Nothing
  where
    snappedResult providerName calls =
      let responses = [resp | Right resp <- map (.response) calls]
       in Just
            Common.SnappedResult
              { provider = providerName,
                distanceMeters = (.getMeters) . highPrecMetersToMeters . sum $ map (.distance) responses,
                snappedPoints = concatMap (.snappedPoints) responses
              }

batchRejections :: Maybe Double -> Batch -> Flow [Common.SnapAttempt]
batchRejections mbConfidenceThreshold batch = case batch.sections of
  [] -> do
    lastRejection <- maybe (pure Nothing) attemptRejection (lastMay batch.attempts)
    if isNothing lastRejection
      then pure []
      else (<> maybeToList lastRejection) . catMaybes <$> mapM attemptRejection (initSafe batch.attempts)
  sectionCalls -> case [rejected call reason Nothing | call <- sectionCalls, Left reason <- [call.response]] of
    [] -> pure []
    sectionFailures -> (<> sectionFailures) . catMaybes <$> mapM attemptRejection batch.attempts
  where
    rejected call reason confidence = Common.SnapAttempt {provider = show call.provider, reason, confidence}
    attemptRejection call = case (call.response, mbConfidenceThreshold) of
      (Left reason, _) -> pure . Just $ rejected call reason Nothing
      (Right _, Nothing) -> pure Nothing
      (Right resp, Just confidenceThreshold) -> do
        postCheckPassed <- Maps.runPostCheck call.provider call.request resp
        pure $
          if
              | not postCheckPassed -> Just $ rejected call (if call.provider `elem` [Maps.OSRM, Maps.Disha] then Common.END_TOO_FAR else Common.POINTS_TOO_FAR_APART) Nothing
              | Maps.isConfidenceOutOfRange confidenceThreshold resp -> Just $ rejected call Common.LOW_CONFIDENCE (Just resp.confidence)
              | otherwise -> Nothing

toSnapCall :: CHExternalApiCallLogs.ExternalApiCallLogs -> Maybe SnapCall
toSnapCall row = do
  apiName <- row.apiName
  payload <- decodePayload @SnapRequestPayload =<< row.requestPayload
  let CH.DateTime createdAt = row.createdAt
      snapResponse snappedPoints mbRouteDistance confidence =
        let distance = case (payload.calculateDistanceFrom, mbRouteDistance) of
              (Nothing, Just routeDistance) -> routeDistance
              _ -> getRouteLinearLength snappedPoints payload.calculateDistanceFrom
         in Right Maps.SnapToRoadResp {distance, distanceWithUnit = convertHighPrecMetersToDistance Meter distance, confidence, snappedPoints}
      parseResponse responsePayload
        | apiName == googleSnapApiName =
          case decodePayload @(GoogleRoads.SnapToRoadResponse' GoogleRoads.SPLocation) responsePayload of
            Just resp | isJust resp.warningMessage -> Left Common.CALL_FAILED
            Just resp -> snapResponse (map (\sp -> LatLong sp.location.latitude sp.location.longitude) resp.snappedPoints) Nothing 1.0
            Nothing -> Left Common.CALL_FAILED
        | otherwise =
          case decodePayload @OSRM.MatchResp responsePayload of
            Just resp | not (null (NE.tail resp.matchings)) -> Left Common.SPLIT_ROUTE
            Just resp ->
              let route = NE.head resp.matchings
                  snappedPoints = map (.getLatLong) route.geometry.coordinates
               in if null payload.points || null snappedPoints
                    then Left Common.CALL_FAILED
                    else snapResponse snappedPoints (Just $ realToFrac route.distance) route.confidence
            Nothing -> Left Common.CALL_FAILED
  pure
    SnapCall
      { provider = if apiName == googleSnapApiName then Maps.Google else Maps.OSRM,
        calledAt = createdAt,
        request = Maps.SnapToRoadReq {points = payload.points, distanceUnit = Meter, calculateDistanceFrom = payload.calculateDistanceFrom},
        response = maybe (Left Common.CALL_FAILED) parseResponse row.responsePayload
      }
