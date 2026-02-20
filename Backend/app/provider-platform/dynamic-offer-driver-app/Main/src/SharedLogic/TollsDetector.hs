{-
 Copyright 2022-23, Juspay India Pvt Ltd
 This program is free software: you can Hedistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.TollsDetector where

import Data.List (nubBy)
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import Domain.Types.Toll
import qualified Kernel.Beam.Functions as B
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.ComputeIntersection
import Storage.CachedQueries.Toll (findAllTollsByMerchantOperatingCity)

tollStartGateTrackingKey :: Id DP.Driver -> Text
tollStartGateTrackingKey driverId = "TollGateTracking:DriverId-" <> driverId.getId

-- This function is called during endRideTransaction to clear all pending tolls
clearTollStartGateBatchCache :: (CacheFlow m r) => Id DP.Driver -> m ()
clearTollStartGateBatchCache driverId = do
  Hedis.del $ tollStartGateTrackingKey driverId

-- Removes the matched toll AND all other possibilities with the same entry gate from pending cache
-- For example, if XL is matched (X entry, L exit), removes XK, XL, XM (all with entry X)
removeMatchedTollFromCache :: (CacheFlow m r) => Id DP.Driver -> [Toll] -> Toll -> m ()
removeMatchedTollFromCache driverId allPendingTolls matchedToll = do
  let remainingPendingTolls = filter (\toll -> toll.tollStartGates /= matchedToll.tollStartGates) allPendingTolls
  if null remainingPendingTolls
    then Hedis.del $ tollStartGateTrackingKey driverId
    else Hedis.setExp (tollStartGateTrackingKey driverId) remainingPendingTolls 21600 -- 6 hours

-- | Validates pending tolls (entry detected, exit not found) against estimated tolls using IDs
-- | Used at end ride to apply toll charges when exit gate was never detected
-- | Returns Nothing if validation fails or no estimate exists (conservative approach for safety)
-- | Now uses toll IDs for exact matching and handles partial toll scenarios (some detected, some not)
checkAndValidatePendingTolls ::
  (CacheFlow m r) =>
  Id DP.Driver ->
  Maybe HighPrecMoney ->
  Maybe [Text] ->
  Maybe [Text] -> -- Estimated toll IDs
  Maybe HighPrecMoney ->
  Maybe [Text] -> -- Already detected toll IDs
  m (Maybe (HighPrecMoney, [Text], [Text]))
checkAndValidatePendingTolls driverId _estimatedTollCharges _estimatedTollNames estimatedTollIds alreadyDetectedCharges alreadyDetectedTollIds = do
  mbPendingTolls :: Maybe [Toll] <- Hedis.safeGet (tollStartGateTrackingKey driverId)

  case (mbPendingTolls, estimatedTollIds, alreadyDetectedTollIds) of
    (Just pendingTolls, Just estIds, Just detectedIds) -> do
      -- Find which estimated tolls are MISSING from already detected ones
      let missingTollIds = filter (`notElem` detectedIds) estIds

      if null missingTollIds
        then do
          -- All estimated tolls already detected
          logInfo "All estimated tolls already detected, no pending tolls to apply"
          return Nothing
        else do
          -- Find pending tolls that match missing IDs
          let matchingPendingTolls = filter (\toll -> getId toll.id `elem` missingTollIds) pendingTolls

          if not $ null matchingPendingTolls
            then do
              -- Calculate charges only for MISSING tolls
              let missingCharges = sum $ map (.price.amount) matchingPendingTolls
                  missingNames = map (.name) matchingPendingTolls
                  missingIds = map (getId . (.id)) matchingPendingTolls

              logInfo $
                "Applying charges for missing tolls: "
                  <> show (zip missingNames (map (.price.amount) matchingPendingTolls))
                  <> " | Already detected: "
                  <> show (fromMaybe 0 alreadyDetectedCharges)

              return $ Just (missingCharges, missingNames, missingIds)
            else do
              logWarning $
                "Missing toll IDs don't match any cached pending tolls. "
                  <> "Missing: "
                  <> show missingTollIds
                  <> " | Cached: "
                  <> show (map (getId . (.id)) pendingTolls)
              return Nothing
    (Just pendingTolls, Just estIds, Nothing) -> do
      -- No tolls detected yet, validate all estimated tolls against pending
      let matchingPendingTolls = filter (\toll -> getId toll.id `elem` estIds) pendingTolls

      if not $ null matchingPendingTolls
        then do
          let charges = sum $ map (.price.amount) matchingPendingTolls
              names = map (.name) matchingPendingTolls
              ids = map (getId . (.id)) matchingPendingTolls

          logInfo $
            "No tolls detected yet, applying estimated tolls: "
              <> show (zip names (map (.price.amount) matchingPendingTolls))

          return $ Just (charges, names, ids)
        else do
          logWarning "Estimated toll IDs don't match any cached pending tolls"
          return Nothing
    (Just pendingTolls, Nothing, _) -> do
      -- Entry detected but NO estimated toll IDs - don't charge without validation
      logWarning $
        "Pending toll found WITHOUT estimate IDs. NOT charging for safety: "
          <> show (map (\t -> (t.name, t.price.amount)) pendingTolls)
      return Nothing
    (Nothing, _, _) ->
      -- No pending tolls in cache
      return Nothing

-- This function is triggered when allTollCombinationsWithStartGates are found intersecting the route to find the exit segment intersection on the further route and return the remaining route after intersection.
getExitTollAndRemainingRoute :: RoutePoints -> [Toll] -> Maybe (RoutePoints, Toll)
getExitTollAndRemainingRoute [] _ = Nothing
getExitTollAndRemainingRoute [_] _ = Nothing
getExitTollAndRemainingRoute _ [] = Nothing
getExitTollAndRemainingRoute (p1 : p2 : ps) tolls = do
  let mbExitToll = find (\Toll {..} -> any (doIntersect (LineSegment p1 p2)) tollEndGates) tolls
  case mbExitToll of
    Just toll -> Just (p2 : ps, toll)
    Nothing -> getExitTollAndRemainingRoute (p2 : ps) tolls

-- This function is responsible for checking the toll start and exit segments intersection on the route and appropriately update the state in case exit segment is not found corresponding to the entry segment while On Ride.
getAggregatedTollChargesAndNamesOnRoute :: (CacheFlow m r) => Maybe (Id DP.Driver) -> RoutePoints -> [Toll] -> (HighPrecMoney, [Text], [Text], Bool, Maybe Bool) -> m (HighPrecMoney, [Text], [Text], Bool, Maybe Bool)
getAggregatedTollChargesAndNamesOnRoute _ [] _ tollChargesAndNamesAndIds = return tollChargesAndNamesAndIds
getAggregatedTollChargesAndNamesOnRoute _ [_] _ tollChargesAndNamesAndIds = return tollChargesAndNamesAndIds
getAggregatedTollChargesAndNamesOnRoute _ _ [] tollChargesAndNamesAndIds = return tollChargesAndNamesAndIds
getAggregatedTollChargesAndNamesOnRoute mbDriverId route@(p1 : p2 : ps) tolls (tollCharges, tollNames, tollIds, tollIsAutoRickshawAllowed, tollIsTwoWheelerAllowed) = do
  let allTollCombinationsWithStartGates = filter (\Toll {..} -> any (doIntersect (LineSegment p1 p2)) tollStartGates) tolls
  if not $ null allTollCombinationsWithStartGates
    then do
      case getExitTollAndRemainingRoute route allTollCombinationsWithStartGates of
        Just (remainingRoute, toll) ->
          getAggregatedTollChargesAndNamesOnRoute
            mbDriverId
            remainingRoute
            tolls
            ( tollCharges + toll.price.amount,
              tollNames <> [toll.name],
              tollIds <> [getId toll.id], -- Accumulate toll IDs
              toll.isAutoRickshawAllowed,
              toll.isTwoWheelerAllowed
            )
        Nothing -> do
          -- Option 1: Accumulate in cache (don't overwrite existing pending tolls)
          whenJust mbDriverId $ \driverId -> do
            mbExistingPendingTolls :: Maybe [Toll] <- Hedis.safeGet (tollStartGateTrackingKey driverId)
            let allPendingTolls = fold mbExistingPendingTolls <> allTollCombinationsWithStartGates
                -- Remove duplicates by ID
                uniquePendingTolls = nubBy (\toll1 toll2 -> getId toll1.id == getId toll2.id) allPendingTolls
            Hedis.setExp (tollStartGateTrackingKey driverId) uniquePendingTolls 21600 -- 6 hours
          return (tollCharges, tollNames, tollIds, tollIsAutoRickshawAllowed, tollIsTwoWheelerAllowed)
    else getAggregatedTollChargesAndNamesOnRoute mbDriverId (p2 : ps) tolls (tollCharges, tollNames, tollIds, tollIsAutoRickshawAllowed, tollIsTwoWheelerAllowed)

{- Author: Khuzema Khomosi
  Best Case Time Complexity - O(No. of points in routes)
  Worst Case Time Complexity - O(No. of points in route * No. of gate segments of eligible tolls)

  This function first finds all the eligible Tolls whose entry segments lie within the Route's bounding box.
  For each two points of the route, it finds all the tolls whose entry segments intersect the two points of the route.
  If it finds some entry segments of tolls intersecting the route points, then it goes further on the route to find the exit segment intersecting the toll.
  Once the exit segment is found, it add's the toll and slices the route further to check for anymore tolls if exists till it reaches the end of the route.

  Note:
  In case of on ride it is possible that the batch of driver waypoints that we have has only the start of the toll and the end of the toll comes later.
  In that case this function maintains the TollCombinationsWithStartGatesInPrevBatch based on driverId to check for it's corresponding exit segment intersection on route coming in later batches.
  Once the exit segment is found, it deletes the TollCombinationsWithStartGatesInPrevBatch & add's the toll and slices the route further to check for anymore tolls if exists till it reaches the end of the route.
-}
getTollInfoOnRoute :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => Id DMOC.MerchantOperatingCity -> Maybe (Id DP.Driver) -> RoutePoints -> m (Maybe (HighPrecMoney, [Text], [Text], Bool, Maybe Bool))
getTollInfoOnRoute merchantOperatingCityId mbDriverId route = do
  tolls <- B.runInReplica $ findAllTollsByMerchantOperatingCity merchantOperatingCityId
  if not $ null tolls
    then do
      let boundingBox = getBoundingBox route
          eligibleTollsThatMaybePresentOnTheRoute = filter (\toll -> any (\lineSegment -> lineSegmentWithinBoundingBox lineSegment boundingBox) toll.tollStartGates) tolls
      case mbDriverId of
        Just driverId -> do
          mbTollCombinationsWithStartGatesInPrevBatch :: Maybe [Toll] <- Hedis.safeGet (tollStartGateTrackingKey driverId)
          case mbTollCombinationsWithStartGatesInPrevBatch of
            Just tollCombinationsWithStartGatesInPrevBatch -> getAggregatedTollChargesConsideringStartSegmentFromPrevBatchWhileOnRide driverId tollCombinationsWithStartGatesInPrevBatch eligibleTollsThatMaybePresentOnTheRoute
            Nothing -> getAggregatedTollCharges route eligibleTollsThatMaybePresentOnTheRoute (0, [], [])
        Nothing -> getAggregatedTollCharges route eligibleTollsThatMaybePresentOnTheRoute (0, [], [])
    else return Nothing
  where
    getAggregatedTollCharges remainingRoute eligibleTollsThatMaybePresentOnTheRoute (tollCharges, tollNames, tollIds) = do
      (aggregatedTollCharges, aggregatedTollNames, aggregatedTollIds, isAutoRickshawAllowed, isTwoWheelerAllowed) <- getAggregatedTollChargesAndNamesOnRoute mbDriverId remainingRoute eligibleTollsThatMaybePresentOnTheRoute (tollCharges, tollNames, tollIds, False, Just False)
      if aggregatedTollCharges > 0 && (not $ null aggregatedTollNames)
        then return $ Just (aggregatedTollCharges, aggregatedTollNames, aggregatedTollIds, isAutoRickshawAllowed, isTwoWheelerAllowed)
        else return Nothing

    getAggregatedTollChargesConsideringStartSegmentFromPrevBatchWhileOnRide driverId tollCombinationsWithStartGatesInPrevBatch eligibleTollsThatMaybePresentOnTheRoute = do
      case getExitTollAndRemainingRoute route tollCombinationsWithStartGatesInPrevBatch of
        Just (remainingRoute, toll) -> do
          -- Remove only the matched toll from cache, keep other pending tolls
          removeMatchedTollFromCache driverId tollCombinationsWithStartGatesInPrevBatch toll
          getAggregatedTollCharges remainingRoute eligibleTollsThatMaybePresentOnTheRoute (toll.price.amount, [toll.name], [getId toll.id])
        Nothing -> do
          logWarning $ "No exit segment of tolls with start segment marked from previous batch found for driverId : " <> driverId.getId <> " TollCombinationsWithStartGatesInPrevBatch : " <> show tollCombinationsWithStartGatesInPrevBatch <> " route : " <> show route
          -- Continue processing current batch for new toll entries even though pending tolls didn't find exits
          getAggregatedTollCharges route eligibleTollsThatMaybePresentOnTheRoute (0, [], [])
