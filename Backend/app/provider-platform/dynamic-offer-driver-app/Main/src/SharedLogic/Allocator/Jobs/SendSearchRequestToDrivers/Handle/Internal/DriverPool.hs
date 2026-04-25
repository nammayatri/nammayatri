{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal.DriverPool
  ( isBatchNumExceedLimit,
    incrementBatchNum,
    getPoolBatchNum,
    module Reexport,
    PrepareDriverPoolBatchEntity (..),
    incrementPoolRadiusStep,
    incrementDriverRequestCount,
    getPoolRadiusStep,
    previouslyAttemptedDrivers,
    checkRequestCount,
    isBookAny,
    makeTaggedDriverPool,
    splitSilentDriversAndSortWithDistance,
    previouslyAttemptedDriversKey,
  )
where

import qualified Control.Monad as CM
import Data.Aeson as A
import Data.Aeson.Types as A
import qualified Data.List as DL
import qualified Domain.Types as DVST
import qualified Domain.Types.Common as DriverInfo
import Domain.Types.DriverPoolConfig
import Domain.Types.MerchantOperatingCity (MerchantOperatingCity)
import Domain.Types.Person (Driver)
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.SearchTry as DST
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Lib.Utils (pushToKafka)
import Kernel.Storage.Clickhouse.Config (ClickhouseFlow)
import qualified Kernel.Storage.ClickhouseV2 as CHV2
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.DatastoreLatencyCalculator
import qualified Lib.Yudhishthira.Tools.DebugLog as LYDL
import qualified Lib.Yudhishthira.Tools.Utils as LYTU
import qualified Lib.Yudhishthira.Types as LYT
import SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal.DriverPool.Config as Reexport
import SharedLogic.DriverPool
import Storage.Beam.Yudhishthira ()
import qualified Storage.Queries.SearchRequest as QSR
import Tools.DynamicLogic

isBatchNumExceedLimit ::
  ( CacheFlow m r
  ) =>
  DriverPoolConfig ->
  Id DST.SearchTry ->
  m Bool
isBatchNumExceedLimit driverPoolConfig searchTryId = do
  let maxNumberOfBatches = driverPoolConfig.maxNumberOfBatches
  currentBatchNum <- getPoolBatchNum searchTryId
  return $ currentBatchNum >= maxNumberOfBatches

previouslyAttemptedDriversKey :: Id DST.SearchTry -> Maybe Bool -> Text
previouslyAttemptedDriversKey searchTryId consideOnRideDrivers = do
  case consideOnRideDrivers of
    Just consideOnRideDrivers' -> "Driver-Offer:PreviouslyAttemptedDrivers:SearchTryId-" <> searchTryId.getId <> ":consideOnRideDrivers-" <> show consideOnRideDrivers'
    Nothing -> "Driver-Offer:PreviouslyAttemptedDrivers:SearchTryId-" <> searchTryId.getId

splitSilentDriversAndSortWithDistance :: [DriverPoolWithActualDistResult] -> [DriverPoolWithActualDistResult]
splitSilentDriversAndSortWithDistance drivers = do
  let (silentDrivers, activeDrivers) = bimap (sortOn (.actualDistanceToPickup)) (sortOn (.actualDistanceToPickup)) $ DL.partition ((== Just DriverInfo.SILENT) . (.driverPoolResult.mode)) drivers
  activeDrivers <> silentDrivers

previouslyAttemptedDrivers ::
  ( Redis.HedisFlow m r
  ) =>
  Id DST.SearchTry ->
  Maybe Bool ->
  m [DriverPoolWithActualDistResult]
previouslyAttemptedDrivers searchTryId consideOnRideDrivers = do
  Redis.withCrossAppRedis $
    Redis.safeGet (previouslyAttemptedDriversKey searchTryId consideOnRideDrivers)
      >>= maybe whenFoundNothing whenFoundSomething
  where
    whenFoundNothing = do
      logWarning "Unexpected empty driver pool batch cache."
      return []
    whenFoundSomething = \case
      [] -> do
        logWarning "Unexpected empty driver pool batch."
        return []
      a -> return a

makeTaggedDriverPool ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m,
    HasField "enableAPILatencyLogging" r Bool,
    HasField "enableAPIPrometheusMetricLogging" r Bool,
    CHV2.HasClickhouseEnv CHV2.APP_SERVICE_CLICKHOUSE m,
    ClickhouseFlow m r
  ) =>
  Id MerchantOperatingCity ->
  Seconds ->
  DSR.SearchRequest ->
  [DriverPoolWithActualDistResult] ->
  Int ->
  Bool ->
  Maybe [LYT.TagNameValue] ->
  Maybe Int ->
  PoolBatchNum ->
  DriverPoolConfig ->
  Id DST.SearchTry ->
  m (Maybe Int, [DriverPoolWithActualDistResult])
makeTaggedDriverPool mOCityId timeDiffFromUtc searchReq onlyNewDrivers batchSize isOnRidePool customerNammaTags mbPoolingLogicVersion batchNum driverPoolCfg searchTryId = do
  localTime <- getLocalCurrentTime timeDiffFromUtc
  (allLogics, mbVersion) <- getAppDynamicLogic (cast mOCityId) LYT.POOLING localTime mbPoolingLogicVersion Nothing
  updateVersionInSearchReq mbVersion
  let onlyNewDriversWithCustomerInfo = map updateDriverPoolWithActualDistResult onlyNewDrivers
  let taggedDriverPoolInput = TaggedDriverPoolInput {drivers = onlyNewDriversWithCustomerInfo, needOnRideDrivers = isOnRidePool, batchNum}
  logInfo $
    "DriverPreference pooling input: customerNammaTags=" <> show customerNammaTags
      <> " | drivers=["
      <> mconcat
        ( map
            ( \d ->
                "driverId=" <> show d.driverPoolResult.driverId
                  <> " gender="
                  <> show d.driverPoolResult.driverGender
                  <> " customerTags="
                  <> show d.driverPoolResult.customerTags
                  <> "; "
            )
            onlyNewDriversWithCustomerInfo
        )
      <> "]"
  resp <- withTimeAPI "driverPooling" "runLogics" $ LYDL.runLogicsWithDebugLog LYDL.Driver (cast mOCityId) LYT.POOLING (Just searchReq.transactionId) allLogics taggedDriverPoolInput
  sortedPool' <-
    case (A.fromJSON resp.result :: Result TaggedDriverPoolInput) of
      A.Success sortedPoolData -> pure sortedPoolData.drivers
      A.Error err -> do
        logError $ "Error in parsing sortedPoolData - " <> show err
        pure onlyNewDriversWithCustomerInfo
  sortedPool <-
    filterM
      ( \driverPoolResult -> do
          now <- getCurrentTime
          let valueToPut = addUTCTime (fromIntegral driverPoolCfg.singleBatchProcessTime) now
          let fromScore = addUTCTime (-1 * (fromIntegral driverPoolCfg.singleBatchProcessTime)) now
          fork "removeExpiredSearchRequestInfoFromCache" $ removeExpiredSearchRequestInfoFromCache driverPoolCfg.merchantId driverPoolResult.driverPoolResult.driverId
          isLessThenNParallelRequests searchReq.id driverPoolCfg.merchantId driverPoolResult.driverPoolResult.driverId valueToPut driverPoolCfg.maxParallelSearchRequests fromScore
      )
      sortedPool'

  pushTaggedPoolToKafka sortedPool
  return (mbVersion, take batchSize sortedPool)
  where
    updateDriverPoolWithActualDistResult DriverPoolWithActualDistResult {..} =
      DriverPoolWithActualDistResult {driverPoolResult = updateDriverPoolResult driverPoolResult, searchTags = Just $ maybe A.emptyObject LYTU.convertTags searchReq.searchTags, tripDistance = searchReq.estimatedDistance, ..}

    updateDriverPoolResult DriverPoolResult {..} =
      DriverPoolResult {customerTags = Just $ maybe A.emptyObject LYTU.convertTags customerNammaTags, ..}

    updateVersionInSearchReq mbVersion =
      whenJust mbVersion $ \_ -> do
        when (isNothing mbPoolingLogicVersion) $
          QSR.updatePoolingLogicVersion mbVersion searchReq.id

    pushTaggedPoolToKafka taggedPool = do
      pushToKafka
        ( SearchTryBatchPoolData
            { searchTryId = searchTryId.getId,
              driverPoolData = taggedPool,
              filterStage = TaggedPool,
              batchNum = batchNum
            }
        )
        "search-try-driver-tagged-pool-batch"
        searchTryId.getId

poolBatchNumKey :: Id DST.SearchTry -> Text
poolBatchNumKey searchTryId = "Driver-Offer:Allocator:PoolBatchNum:SearchTryId-" <> searchTryId.getId

poolRadiusStepKey :: Id DST.SearchTry -> Text
poolRadiusStepKey searchTryId = "Driver-Offer:Allocator:PoolRadiusStep:SearchTryId-" <> searchTryId.getId

getPoolBatchNum :: (Redis.HedisFlow m r) => Id DST.SearchTry -> m PoolBatchNum
getPoolBatchNum searchTryId = do
  res <- Redis.withCrossAppRedis $ Redis.get (poolBatchNumKey searchTryId)
  case res of
    Just i -> return i
    Nothing -> do
      let expTime = 600
      Redis.withCrossAppRedis $ Redis.setExp (poolBatchNumKey searchTryId) (-1 :: Integer) expTime
      return (-1)

incrementBatchNum ::
  ( Redis.HedisFlow m r
  ) =>
  Id DST.SearchTry ->
  m ()
incrementBatchNum searchTryId = do
  res <- Redis.withCrossAppRedis $ Redis.incr (poolBatchNumKey searchTryId)
  logInfo $ "Increment batch num to " <> show res <> "."
  return ()

getPoolRadiusStep :: (Redis.HedisFlow m r) => Id DST.SearchTry -> m PoolRadiusStep
getPoolRadiusStep searchTryId = do
  res <- Redis.withCrossAppRedis $ Redis.get (poolRadiusStepKey searchTryId)
  case res of
    Just i -> return i
    Nothing -> do
      let expTime = 600
      Redis.withCrossAppRedis $ Redis.setExp (poolRadiusStepKey searchTryId) (0 :: Integer) expTime
      return 0

incrementPoolRadiusStep ::
  ( Redis.HedisFlow m r
  ) =>
  Id DST.SearchTry ->
  m ()
incrementPoolRadiusStep searchTryId = do
  res <- Redis.withCrossAppRedis $ Redis.incr (poolRadiusStepKey searchTryId)
  logInfo $ "Increment radius step to " <> show res <> "."
  return ()

isBookAny :: [DVST.ServiceTierType] -> Bool
isBookAny vehicleServiceTiers = length vehicleServiceTiers > 1

driverRequestCountKey :: Id DST.SearchTry -> Id Driver -> DVST.ServiceTierType -> Text
driverRequestCountKey searchTryId driverId vehicleServiceTier = "Driver-Request-Count-Key:SearchTryId-" <> searchTryId.getId <> ":DriverId-" <> driverId.getId <> ":ServiceTier-" <> show vehicleServiceTier

checkRequestCount :: Redis.HedisFlow m r => Id DST.SearchTry -> Bool -> Id Driver -> DVST.ServiceTierType -> Int -> DriverPoolConfig -> m Bool
checkRequestCount searchTryId isBookAnyRequest driverId vehicleServiceTier serviceTierDowngradeLevel driverPoolConfig =
  maybe True (\count -> (count :: Int) < getDriverRequestCountLimit serviceTierDowngradeLevel isBookAnyRequest driverPoolConfig)
    <$> Redis.withCrossAppRedis (Redis.get (driverRequestCountKey searchTryId driverId vehicleServiceTier))

getDriverRequestCountLimit :: Int -> Bool -> DriverPoolConfig -> Int
getDriverRequestCountLimit serviceTierDowngradeLevel isBookAnyRequest driverPoolConfig =
  if serviceTierDowngradeLevel < 0 && isBookAnyRequest -- if BookAny and serviceTierDowngradeLevel is less than 0, then limit is 1
    then 1
    else driverPoolConfig.driverRequestCountLimit

incrementDriverRequestCount :: (Redis.HedisFlow m r) => [DriverPoolWithActualDistResult] -> Id DST.SearchTry -> m ()
incrementDriverRequestCount finalPoolBatch searchTryId = do
  CM.mapM_
    ( \dpr ->
        Redis.withCrossAppRedis do
          void $ Redis.incr (driverRequestCountKey searchTryId dpr.driverPoolResult.driverId dpr.driverPoolResult.serviceTier)
          Redis.expire (driverRequestCountKey searchTryId dpr.driverPoolResult.driverId dpr.driverPoolResult.serviceTier) 7200
    )
    finalPoolBatch

data PrepareDriverPoolBatchEntity = PrepareDriverPoolBatchEntity
  { currentDriverPoolBatch :: [DriverPoolWithActualDistResult],
    poolType :: PoolType,
    nextScheduleTime :: Maybe Seconds,
    currentDriverPoolBatchOnRide :: [DriverPoolWithActualDistResult]
  }
