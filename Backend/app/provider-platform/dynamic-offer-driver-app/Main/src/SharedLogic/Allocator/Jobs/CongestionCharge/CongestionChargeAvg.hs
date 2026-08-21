{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Allocator.Jobs.CongestionCharge.CongestionChargeAvg
  ( calculateCongestionChargeAvgTaxi,
  )
where

import qualified Data.Map.Strict as Map
import qualified Domain.Types.ServiceTierType as DServiceTierType
import qualified Domain.Types.VehicleCategory as DVC
import Domain.Types.VehicleVariant (castServiceTierToVehicleCategory)
import Kernel.External.Types (SchedulerFlow)
import Kernel.Prelude
import qualified Kernel.Storage.ClickhouseV2 as CH
import Kernel.Storage.Esqueleto.Config
import qualified Kernel.Storage.Hedis.Queries as Hedis
import Kernel.Utils.Common
import Lib.Scheduler
import SharedLogic.Allocator (AllocatorJobType (..), CongestionChargeCalculationRequestJobData (..))
import SharedLogic.DynamicPricing
import qualified Storage.Clickhouse.Estimate as Est

-- Rolls per-tier (row-level) congestion averages up into per-VehicleCategory averages,
-- weighted by how many estimates backed each tier's average (so a category with mostly
-- SEDAN estimates and a handful of TAXI ones isn't skewed 50/50 towards TAXI).
-- Rows with no congestion signal (Nothing avg, or zero count) are dropped before weighting.
rollUpByCategory :: Ord k => [(k, Maybe Double, Int, DServiceTierType.ServiceTierType)] -> [(k, DVC.VehicleCategory, Double)]
rollUpByCategory rows =
  [ (loc, cat, total / fromIntegral cnt)
    | ((loc, cat), (total, cnt)) <- Map.toList weighted,
      cnt > (0 :: Int)
  ]
  where
    weighted =
      foldl'
        ( \acc (loc, mbAvg, cnt, tier) ->
            case mbAvg of
              Just avg
                | cnt > 0 ->
                  let cat = castServiceTierToVehicleCategory tier
                   in Map.insertWith (\(t1, c1) (t2, c2) -> (t1 + t2, c1 + c2)) (loc, cat) (avg * fromIntegral cnt, cnt) acc
              _ -> acc
        )
        Map.empty
        rows

calculateCongestionChargeAvgTaxi ::
  ( SchedulerFlow r,
    EsqDBReplicaFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m
  ) =>
  Job 'CongestionCharge ->
  m ExecutionResult
calculateCongestionChargeAvgTaxi Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  now <- getCurrentTime
  let CongestionChargeCalculationRequestJobData {..} = jobInfo.jobData
  let from = addUTCTime (intToNominalDiffTime (calculationDataIntervalInMin * (-60))) now -----------multiply by -60 to take past timing
  let nextScheduleT = addUTCTime (intToNominalDiffTime (scheduleTimeIntervalInMin * 60)) now
  calculateAndUpdateCityCongestion now from congestionChargeCalculationTTLInSec
  calculateAndUpdateGeohashAndDistanceBinCongestion now from congestionChargeCalculationTTLInSec
  rawResult <- Est.calulateCongestionByGeohash from now
  let rolledUpResult = rollUpByCategory [(geohash, avg, cnt, tier) | (Just geohash, avg, cnt, tier) <- rawResult]
  logInfo $ "CongestionChargeCalculation clickhouse result : -" <> show rawResult <> " rolled up by category : -" <> show rolledUpResult
  mapM_ (updateGeohashCongestion congestionChargeCalculationTTLInSec) rolledUpResult
  return (ReSchedule nextScheduleT)

updateGeohashCongestion ::
  ( SchedulerFlow r,
    EsqDBReplicaFlow m r,
    CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Int ->
  (Text, DVC.VehicleCategory, Double) ->
  m ()
updateGeohashCongestion congestionChargeCalculationTTLInSec (geohash, vehicleCategory, congestionMultiplier) = do
  let key = mkCongestionKeyWithGeohash geohash (Just vehicleCategory)
      key1 = mkCongestionKeyWithGeohashPast geohash (Just vehicleCategory)
  (val :: Maybe Double) <- Hedis.withCrossAppRedis $ Hedis.get key
  Hedis.withCrossAppRedis $ Hedis.setExp key1 val congestionChargeCalculationTTLInSec
  Hedis.withCrossAppRedis $ Hedis.setExp key congestionMultiplier congestionChargeCalculationTTLInSec

calculateAndUpdateCityCongestion ::
  ( SchedulerFlow r,
    EsqDBReplicaFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m
  ) =>
  UTCTime ->
  UTCTime ->
  Int ->
  m ()
calculateAndUpdateCityCongestion now from congestionChargeCalculationTTLInSec = do
  rawValue <- Est.calulateCongestionByCity from now
  let rolledUpValue = rollUpByCategory [(cityId.getId, avg, cnt, tier) | (cityId, avg, cnt, tier) <- rawValue]
  logInfo $ "CongestionChargeCalculation clickhouse result : -" <> show rawValue <> " rolled up by category : -" <> show rolledUpValue
  mapM_ (updateCityCongestion congestionChargeCalculationTTLInSec) rolledUpValue

updateCityCongestion ::
  ( SchedulerFlow r,
    EsqDBReplicaFlow m r,
    CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Int ->
  (Text, DVC.VehicleCategory, Double) ->
  m ()
updateCityCongestion congestionChargeCalculationTTLInSec (city, vehicleCategory, congestionMultiplier) = do
  let key = mkCongestionKeyWithCity city (Just vehicleCategory)
      key1 = mkCongestionKeyWithCityPast city (Just vehicleCategory)
  (val :: Maybe Double) <- Hedis.withCrossAppRedis $ Hedis.get key
  Hedis.withCrossAppRedis $ Hedis.setExp key1 val congestionChargeCalculationTTLInSec
  Hedis.withCrossAppRedis $ Hedis.setExp key congestionMultiplier congestionChargeCalculationTTLInSec

calculateAndUpdateGeohashAndDistanceBinCongestion ::
  ( SchedulerFlow r,
    EsqDBReplicaFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m
  ) =>
  UTCTime ->
  UTCTime ->
  Int ->
  m ()
calculateAndUpdateGeohashAndDistanceBinCongestion now from congestionChargeCalculationTTLInSec = do
  rawResult <- Est.calulateCongestionByGeohashAndDistanceBin from now
  let rolledUpResult = rollUpByCategory [((geohash, distanceBin), avg, cnt, tier) | (Just geohash, avg, cnt, tier, distanceBin) <- rawResult]
  logInfo $ "CongestionChargeCalculation clickhouse result : -" <> show rawResult <> " rolled up by category : -" <> show rolledUpResult
  mapM_ (updateGeohashAndDistanceBinCongestion congestionChargeCalculationTTLInSec) rolledUpResult

updateGeohashAndDistanceBinCongestion ::
  ( SchedulerFlow r,
    EsqDBReplicaFlow m r,
    CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Int ->
  ((Text, Text), DVC.VehicleCategory, Double) ->
  m ()
updateGeohashAndDistanceBinCongestion congestionChargeCalculationTTLInSec ((geohash, distanceBin), vehicleCategory, congestionMultiplier) = do
  let key = mkCongestionKeyWithGeohashAndDistanceBin geohash distanceBin (Just vehicleCategory)
      key1 = mkCongestionKeyWithGeohashAndDistanceBinPast geohash distanceBin (Just vehicleCategory)
  (val :: Maybe Double) <- Hedis.withCrossAppRedis $ Hedis.get key
  Hedis.withCrossAppRedis $ Hedis.setExp key1 val congestionChargeCalculationTTLInSec
  Hedis.withCrossAppRedis $ Hedis.setExp key congestionMultiplier congestionChargeCalculationTTLInSec
