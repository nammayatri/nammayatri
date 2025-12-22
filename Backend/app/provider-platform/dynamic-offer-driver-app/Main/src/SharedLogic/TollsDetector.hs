{-
 Copyright 2022-23, Juspay India Pvt Ltd
 This program is free software: you can Hedistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.TollsDetector where

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

-- This function is called during endRideTransaction & If the exit segment is found for TollCombinationsWithStartGatesInPrevBatch in case of On Ride.
clearTollStartGateBatchCache :: (CacheFlow m r) => Id DP.Driver -> m ()
clearTollStartGateBatchCache driverId = do
  Hedis.del $ tollStartGateTrackingKey driverId

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
getAggregatedTollChargesAndNamesOnRoute :: (CacheFlow m r) => Maybe (Id DP.Driver) -> RoutePoints -> [Toll] -> (HighPrecMoney, [Text], Bool, Maybe Bool) -> m (HighPrecMoney, [Text], Bool, Maybe Bool)
getAggregatedTollChargesAndNamesOnRoute _ [] _ tollChargesAndNames = return tollChargesAndNames
getAggregatedTollChargesAndNamesOnRoute _ [_] _ tollChargesAndNames = return tollChargesAndNames
getAggregatedTollChargesAndNamesOnRoute _ _ [] tollChargesAndNames = return tollChargesAndNames
getAggregatedTollChargesAndNamesOnRoute mbDriverId route@(p1 : p2 : ps) tolls (tollCharges, tollNames, tollIsAutoRickshawAllowed, tollIsTwoWheelerAllowed) = do
  let allTollCombinationsWithStartGates = filter (\Toll {..} -> any (doIntersect (LineSegment p1 p2)) tollStartGates) tolls
  if not $ null allTollCombinationsWithStartGates
    then do
      case getExitTollAndRemainingRoute route allTollCombinationsWithStartGates of
        Just (remainingRoute, toll) -> do
          whenJust mbDriverId $ \driverId ->
            logError $
              "TollFlow: toll exit gates intersection found (same batch)"
                <> " driverId="
                <> driverId.getId
                <> " toll="
                <> toll.name
          getAggregatedTollChargesAndNamesOnRoute mbDriverId remainingRoute tolls (tollCharges + toll.price.amount, tollNames <> [toll.name], toll.isAutoRickshawAllowed, toll.isTwoWheelerAllowed)
        Nothing -> do
          whenJust mbDriverId $ \driverId -> do
            Hedis.setExp (tollStartGateTrackingKey driverId) allTollCombinationsWithStartGates 21600 -- 6 hours
            logError $
              "TollFlow: toll start gates intersection (without exit) cached"
                <> " driverId="
                <> driverId.getId
                <> " startGates="
                <> show (map (.name) allTollCombinationsWithStartGates)
                <> " segment="
                <> show (p1, p2)
          return (tollCharges, tollNames, tollIsAutoRickshawAllowed, tollIsTwoWheelerAllowed)
    else getAggregatedTollChargesAndNamesOnRoute mbDriverId (p2 : ps) tolls (tollCharges, tollNames, tollIsAutoRickshawAllowed, tollIsTwoWheelerAllowed)

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
getTollInfoOnRoute :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => Id DMOC.MerchantOperatingCity -> Maybe (Id DP.Driver) -> RoutePoints -> m (Maybe (HighPrecMoney, [Text], Bool, Maybe Bool))
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
            Nothing -> getAggregatedTollCharges route eligibleTollsThatMaybePresentOnTheRoute (0, [])
        Nothing -> getAggregatedTollCharges route eligibleTollsThatMaybePresentOnTheRoute (0, [])
    else return Nothing
  where
    getAggregatedTollCharges remainingRoute eligibleTollsThatMaybePresentOnTheRoute (tollCharges, tollNames) = do
      (aggregatedTollCharges, aggregatedTollNames, isAutoRickshawAllowed, isTwoWheelerAllowed) <- getAggregatedTollChargesAndNamesOnRoute mbDriverId remainingRoute eligibleTollsThatMaybePresentOnTheRoute (tollCharges, tollNames, False, Just False)
      if aggregatedTollCharges > 0 && (not $ null aggregatedTollNames)
        then return $ Just (aggregatedTollCharges, aggregatedTollNames, isAutoRickshawAllowed, isTwoWheelerAllowed)
        else return Nothing

    getAggregatedTollChargesConsideringStartSegmentFromPrevBatchWhileOnRide driverId tollCombinationsWithStartGatesInPrevBatch eligibleTollsThatMaybePresentOnTheRoute = do
      case getExitTollAndRemainingRoute route tollCombinationsWithStartGatesInPrevBatch of
        Just (remainingRoute, toll) -> do
          logError $
            "TollFlow: toll exit gates intersection found for cached starts (previous batch)"
              <> " driverId="
              <> driverId.getId
              <> " toll="
              <> toll.name
          clearTollStartGateBatchCache driverId
          getAggregatedTollCharges remainingRoute eligibleTollsThatMaybePresentOnTheRoute (toll.price.amount, [toll.name])
        Nothing -> do
          logError $ "TollFlow: No exit segment of tolls with start segment marked from previous batch found for driverId : " <> driverId.getId <> " TollCombinationsWithStartGatesInPrevBatch : " <> show tollCombinationsWithStartGatesInPrevBatch <> " route : " <> show route
          return Nothing
