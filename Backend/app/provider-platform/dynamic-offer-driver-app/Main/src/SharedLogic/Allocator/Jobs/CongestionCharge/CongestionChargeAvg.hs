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

import Domain.Types.Extra.LeanFlow (LeanFlowFeature (CONGESTION_CHARGE))
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Kernel.External.Types (SchedulerFlow)
import Kernel.Prelude
import qualified Kernel.Storage.ClickhouseV2 as CH
import Kernel.Storage.Esqueleto.Config
import qualified Kernel.Storage.Hedis.Queries as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Scheduler
import SharedLogic.Allocator (AllocatorJobType (..), CongestionChargeCalculationRequestJobData (..))
import SharedLogic.DynamicPricing
import qualified Storage.CachedQueries.SystemConfigs.LeanFlow as CQLF
import qualified Storage.Clickhouse.Estimate as Est

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
  let nextScheduleT = addUTCTime (intToNominalDiffTime (scheduleTimeIntervalInMin * 60)) now
  congestionChargeExcluded <- CQLF.isFeatureExcluded CONGESTION_CHARGE
  unless congestionChargeExcluded $ do
    let from = addUTCTime (intToNominalDiffTime (calculationDataIntervalInMin * (-60))) now -----------multiply by -60 to take past timing
    calculateAndUpdateCityCongestion now from congestionChargeCalculationTTLInSec congestionAvgDampingFactor
    calculateAndUpdateGeohashAndDistanceBinCongestion now from congestionChargeCalculationTTLInSec congestionAvgDampingFactor
    result <- Est.calulateCongestionByGeohash from now
    -- query2Result <- SRFD.calulateAcceptanceCountByGeohashAndServiceTier from now
    -- let queryResult = SRFD.concatFun query1Result query2Result
    logInfo $ "CongestionChargeCalculation clickhouse result : -" <> show result
    mapM_ (updateGeohashCongestion congestionChargeCalculationTTLInSec congestionAvgDampingFactor) result
  return (ReSchedule nextScheduleT)

updateGeohashCongestion ::
  ( SchedulerFlow r,
    EsqDBReplicaFlow m r,
    CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Int ->
  Maybe Double ->
  (Maybe Text, Maybe Double) ->
  m ()
updateGeohashCongestion congestionChargeCalculationTTLInSec dampingFactor (geohash', congestionMultiplier) = do
  let geohash = fromMaybe "" geohash'
      key = mkCongestionKeyWithGeohash geohash
      key1 = mkCongestionKeyWithGeohashPast geohash
  (val :: Maybe Double) <- Hedis.withCrossAppRedis $ Hedis.get key
  Hedis.withCrossAppRedis $ Hedis.setExp key1 val congestionChargeCalculationTTLInSec
  whenJust congestionMultiplier (\cm -> Hedis.withCrossAppRedis $ Hedis.setExp key (applyDamping dampingFactor cm) congestionChargeCalculationTTLInSec)

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
  Maybe Double ->
  m ()
calculateAndUpdateCityCongestion now from congestionChargeCalculationTTLInSec dampingFactor = do
  value <- Est.calulateCongestionByCity from now
  -- query2Result <- SRFD.calulateAcceptanceCountByCityAndServiceTier from now
  -- let queryResult = SRFD.concatFun' query2Result query1Result
  logInfo $ "CongestionChargeCalculation clickhouse result : -" <> show value
  mapM_ (updateCityCongestion congestionChargeCalculationTTLInSec dampingFactor) value

updateCityCongestion ::
  ( SchedulerFlow r,
    EsqDBReplicaFlow m r,
    CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Int ->
  Maybe Double ->
  (Id DMOC.MerchantOperatingCity, Maybe Double) ->
  m ()
updateCityCongestion congestionChargeCalculationTTLInSec dampingFactor (cityId, congestionMultiplier) = do
  let city = cityId.getId
      key = mkCongestionKeyWithCity city
      key1 = mkCongestionKeyWithCityPast city
  (val :: Maybe Double) <- Hedis.withCrossAppRedis $ Hedis.get key
  Hedis.withCrossAppRedis $ Hedis.setExp key1 val congestionChargeCalculationTTLInSec
  whenJust congestionMultiplier (\cm -> Hedis.withCrossAppRedis $ Hedis.setExp key (applyDamping dampingFactor cm) congestionChargeCalculationTTLInSec)

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
  Maybe Double ->
  m ()
calculateAndUpdateGeohashAndDistanceBinCongestion now from congestionChargeCalculationTTLInSec dampingFactor = do
  result <- Est.calulateCongestionByGeohashAndDistanceBin from now
  -- query2Result <- SRFD.calulateAcceptanceCountByGeohashAndServiceTierAndDistanceBin from now
  -- let queryResult = SRFD.concatFun'' query1Result query2Result
  logInfo $ "CongestionChargeCalculation clickhouse result : -" <> show result
  mapM_ (updateGeohashAndDistanceBinCongestion congestionChargeCalculationTTLInSec dampingFactor) result

updateGeohashAndDistanceBinCongestion ::
  ( SchedulerFlow r,
    EsqDBReplicaFlow m r,
    CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Int ->
  Maybe Double ->
  (Maybe Text, Maybe Double, Text) ->
  m ()
updateGeohashAndDistanceBinCongestion congestionChargeCalculationTTLInSec dampingFactor (geohash', congestionMultiplier, distanceBin) = do
  let geohash = fromMaybe "" geohash'
      key = mkCongestionKeyWithGeohashAndDistanceBin geohash distanceBin
      key1 = mkCongestionKeyWithGeohashAndDistanceBinPast geohash distanceBin
  (val :: Maybe Double) <- Hedis.withCrossAppRedis $ Hedis.get key
  Hedis.withCrossAppRedis $ Hedis.setExp key1 val congestionChargeCalculationTTLInSec
  whenJust congestionMultiplier (\cm -> Hedis.withCrossAppRedis $ Hedis.setExp key (applyDamping dampingFactor cm) congestionChargeCalculationTTLInSec)

-- feedback-loop damping: the multiplier written here becomes next cycle's
-- input, so an undamped average can self-reinforce a spike. factor 1.0 (or
-- Nothing) keeps historical behavior.
applyDamping :: Maybe Double -> Double -> Double
applyDamping mbFactor cm = 1 + (cm - 1) * fromMaybe 1.0 mbFactor
