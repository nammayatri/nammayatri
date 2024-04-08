{-
 Copyright 2022-23, Juspay India Pvt Ltd
 This program is free software: you can Hedistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.TollsDetector where

import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
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

doTollGateExistOnRoute :: [LineSegment] -> RoutePoints -> Bool
doTollGateExistOnRoute [] _ = False
doTollGateExistOnRoute (g1 : gs) route =
  checkIntersection route g1 || doTollGateExistOnRoute gs route

doTollExistsOnRoute :: RoutePoints -> Toll -> Bool
doTollExistsOnRoute route Toll {..} =
  doTollGateExistOnRoute tollStartGates route
    && ( tollStartGates == tollEndGates
           || doTollGateExistOnRoute tollEndGates route
       )

getTollChargesOnRoute :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => Id DMOC.MerchantOperatingCity -> RoutePoints -> m (Maybe HighPrecMoney)
getTollChargesOnRoute merchantOperatingCityId route = do
  tolls <- B.runInReplica $ findAllTollsByMerchantOperatingCity merchantOperatingCityId
  let tollChargesAggregate =
        foldl
          ( \tollCharges toll -> do
              if doTollExistsOnRoute route toll
                then tollCharges + toll.price.amount
                else tollCharges
          )
          0
          tolls
  return $ if tollChargesAggregate > 0 then Just tollChargesAggregate else Nothing

-----------------------------------------------------------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------- For End Ride Batch Step -----------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------

tollStartGateTrackingKey :: Id DP.Driver -> Text
tollStartGateTrackingKey driverId = "TollGateTracking:DriverId-" <> driverId.getId

clearTollStartGateBatchCache :: (CacheFlow m r) => Id DP.Driver -> m ()
clearTollStartGateBatchCache driverId = do
  Hedis.del $ tollStartGateTrackingKey driverId

getPreviousStartGateTollChargeIfExists :: (CacheFlow m r) => Id DP.Driver -> RoutePoints -> m HighPrecMoney
getPreviousStartGateTollChargeIfExists driverId route = do
  mbPrevStartToll :: Maybe Toll <- Hedis.safeGet (tollStartGateTrackingKey driverId)
  case mbPrevStartToll of
    Just prevStartToll ->
      if prevStartToll.tollStartGates == prevStartToll.tollEndGates || doTollGateExistOnRoute prevStartToll.tollEndGates route
        then return $ prevStartToll.price.amount
        else return 0
    Nothing -> return 0

doTollExistsOnRouteBatchStep :: (CacheFlow m r) => Id DP.Driver -> RoutePoints -> Toll -> m Bool
doTollExistsOnRouteBatchStep driverId route toll@Toll {..} = do
  let tollStartExists = doTollGateExistOnRoute tollStartGates route
      tollEndExists = tollStartGates == tollEndGates || doTollGateExistOnRoute tollEndGates route
  when (tollStartExists && not tollEndExists) $ Hedis.setExp (tollStartGateTrackingKey driverId) toll 21600 -- 6 hours
  return $ tollStartExists && tollEndExists

getTollChargesOnRouteInBatchStep ::
  (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  Id DP.Driver ->
  RoutePoints ->
  m (Maybe HighPrecMoney)
getTollChargesOnRouteInBatchStep merchantOperatingCityId driverId route = do
  prevTollAmount <- getPreviousStartGateTollChargeIfExists driverId route
  tolls <- B.runInReplica $ findAllTollsByMerchantOperatingCity merchantOperatingCityId
  tollChargesAggregate <-
    foldlM
      ( \tollCharges toll -> do
          tollExists <- doTollExistsOnRouteBatchStep driverId route toll
          if tollExists
            then return $ tollCharges + toll.price.amount
            else return $ tollCharges
      )
      prevTollAmount
      tolls
  return $ if tollChargesAggregate > 0 then Just tollChargesAggregate else Nothing
