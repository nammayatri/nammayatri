{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Allocator.Jobs.SupplyDemand.SupplyDemandRatio
  ( calculateSupplyDemand,
  )
where

import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.ServiceTierType as DServiceTierType
import Kernel.External.Types (SchedulerFlow)
import Kernel.Prelude
import qualified Kernel.Storage.ClickhouseV2 as CH
import Kernel.Storage.Esqueleto.Config
import qualified Kernel.Storage.Hedis.Queries as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Scheduler
import SharedLogic.Allocator (AllocatorJobType (..), SupplyDemandRequestJobData (..))
import SharedLogic.DynamicPricing (mkActualQARKeyWithCity, mkActualQARKeyWithGeohash, mkSupplyDemandRatioKeyWithGeohash)
import qualified Storage.Clickhouse.SearchRequestForDriver as SRFD

calculateSupplyDemand ::
  ( SchedulerFlow r,
    EsqDBReplicaFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m
  ) =>
  Job 'SupplyDemand ->
  m ExecutionResult
calculateSupplyDemand Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  now <- getCurrentTime
  let SupplyDemandRequestJobData {..} = jobInfo.jobData
  let from = addUTCTime (intToNominalDiffTime (calculationDataIntervalInMin * (-60))) now -----------multiply by -60 to take past timing
  let nextScheduleT = addUTCTime (intToNominalDiffTime (scheduleTimeIntervalInMin * 60)) now
  calculateAndUpdateCityQAR now from supplyDemandRatioTTLInSec
  query1Result <- SRFD.calulateSupplyDemandByGeohashAndServiceTier from now
  query2Result <- SRFD.calulateAcceptanceCountByGeohashAndServiceTier from now
  let queryResult = SRFD.concatFun query1Result query2Result
  logInfo $ "SupplyDemandRatio clickhouse result : -" <> show queryResult
  mapM_ (updateSupplyDemandRatio supplyDemandRatioTTLInSec) queryResult
  return (ReSchedule nextScheduleT)

updateSupplyDemandRatio ::
  ( SchedulerFlow r,
    EsqDBReplicaFlow m r,
    CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Int ->
  (Maybe Text, Int, Int, Int, DServiceTierType.ServiceTierType) ->
  m ()
updateSupplyDemandRatio supplyDemandRatioTTLInSec (geohash', acceptanceCount, supplyCount, demandCount, vehicleServiceTier) = do
  let geohash = fromMaybe "" geohash'
      key1 = mkSupplyDemandRatioKeyWithGeohash geohash vehicleServiceTier
      value1 :: Double = if demandCount == 0 then 0.0 else fromIntegral supplyCount / fromIntegral demandCount
      key2 = mkActualQARKeyWithGeohash geohash vehicleServiceTier
      value2 :: Double = if demandCount == 0 then 0.0 else fromIntegral acceptanceCount / fromIntegral demandCount
  Hedis.withCrossAppRedis $ Hedis.setExp key1 value1 supplyDemandRatioTTLInSec
  if demandCount < 10
    then Hedis.withCrossAppRedis $ Hedis.del key2
    else Hedis.withCrossAppRedis $ Hedis.setExp key2 value2 supplyDemandRatioTTLInSec

calculateAndUpdateCityQAR ::
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
calculateAndUpdateCityQAR now from supplyDemandRatioTTLInSec = do
  query1Result <- SRFD.calulateDemandByCityAndServiceTier from now
  query2Result <- SRFD.calulateAcceptanceCountByCityAndServiceTier from now
  let queryResult = SRFD.concatFun' query2Result query1Result
  logInfo $ "SupplyDemandRatio clickhouse result : -" <> show queryResult
  mapM_ (updateCityActualQAR supplyDemandRatioTTLInSec) queryResult

updateCityActualQAR ::
  ( SchedulerFlow r,
    EsqDBReplicaFlow m r,
    CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Int ->
  (Id DMOC.MerchantOperatingCity, Int, Int, DServiceTierType.ServiceTierType) ->
  m ()
updateCityActualQAR supplyDemandRatioTTLInSec (cityId, acceptanceCount, demandCount, vehicleServiceTier) = do
  let city = cityId.getId
      key = mkActualQARKeyWithCity city vehicleServiceTier
      value :: Double = if demandCount < 50 then 0.0 else fromIntegral acceptanceCount / fromIntegral demandCount
  Hedis.withCrossAppRedis $ Hedis.setExp key value supplyDemandRatioTTLInSec
