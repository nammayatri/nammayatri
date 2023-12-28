{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.MerchantConfig
  ( updateCustomerFraudCounters,
    updateCancelledByDriverFraudCounters,
    updateSearchFraudCounters,
    updateTotalRidesCounters,
    updateTotalRidesInWindowCounters,
    anyFraudDetected,
    mkCancellationKey,
    mkCancellationByDriverKey,
    blockCustomer,
  )
where

import Data.Foldable.Extra
import qualified Domain.Types.Booking.Type as BT
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantConfig as DMC
import qualified Domain.Types.Person as Person
import Kernel.Beam.Functions as B
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Kernel.Utils.SlidingWindowCounters as SWC
import qualified Storage.CachedQueries.Merchant.MerchantServiceUsageConfig as CMSUC
import qualified Storage.Queries.Booking as QB
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.RegistrationToken as RT
import Tools.Auth (authTokenCacheKey)

data Factors = MoreCancelling | MoreSearching | MoreCancelledByDriver | TotalRides | TotalRidesInWindow

mkCancellationKey :: Text -> Text -> Text
mkCancellationKey ind idtxt = "Customer:CancellationCount:" <> idtxt <> ":" <> ind

mkCancellationByDriverKey :: Text -> Text -> Text
mkCancellationByDriverKey ind idtxt = "Customer:CancellationByDriverCount:" <> idtxt <> ":" <> ind

mkTotalRidesKey :: Text -> Text
mkTotalRidesKey idtxt = "Customer:TotalRidesCount:" <> idtxt

mkSearchCounterKey :: Text -> Text -> Text
mkSearchCounterKey ind idtxt = "Customer:SearchCounter:" <> idtxt <> ":" <> ind

mkRideWindowCountKey :: Text -> Text -> Text
mkRideWindowCountKey ind idtxt = "Customer:RidesCount:" <> idtxt <> ":" <> ind

updateSearchFraudCounters :: (CacheFlow m r, MonadFlow m) => Id Person.Person -> [DMC.MerchantConfig] -> m ()
updateSearchFraudCounters riderId merchantConfigs = Redis.withNonCriticalCrossAppRedis $ do
  mapM_ (\mc -> incrementCount mc.id.getId mc.fraudSearchCountWindow) merchantConfigs
  where
    incrementCount ind = SWC.incrementWindowCount (mkSearchCounterKey ind riderId.getId)

updateCancelledByDriverFraudCounters :: (CacheFlow m r, MonadFlow m) => Id Person.Person -> [DMC.MerchantConfig] -> m ()
updateCancelledByDriverFraudCounters riderId merchantConfigs = Redis.withNonCriticalCrossAppRedis $ do
  mapM_ (\mc -> incrementCount mc.id.getId mc.fraudBookingCancelledByDriverCountWindow) merchantConfigs
  where
    incrementCount ind = SWC.incrementWindowCount (mkCancellationByDriverKey ind riderId.getId)

updateCustomerFraudCounters :: (CacheFlow m r, MonadFlow m) => Id Person.Person -> [DMC.MerchantConfig] -> m ()
updateCustomerFraudCounters riderId merchantConfigs = Redis.withNonCriticalCrossAppRedis $ do
  mapM_ (\mc -> incrementCount mc.id.getId mc.fraudBookingCancellationCountWindow) merchantConfigs
  where
    incrementCount ind = SWC.incrementWindowCount (mkCancellationKey ind riderId.getId)

updateTotalRidesCounters :: (HedisFlow m r, CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => Id Person.Person -> m ()
updateTotalRidesCounters riderId = Redis.withNonCriticalCrossAppRedis $ do
  _ <- getTotalRidesCount riderId
  let key = mkTotalRidesKey riderId.getId
  void $ Redis.incr key

updateTotalRidesInWindowCounters :: (CacheFlow m r, MonadFlow m) => Id Person.Person -> [DMC.MerchantConfig] -> m ()
updateTotalRidesInWindowCounters riderId merchantConfigs = Redis.withNonCriticalCrossAppRedis $ do
  mapM_ (\mc -> incrementCount mc.id.getId mc.fraudRideCountWindow) merchantConfigs
  where
    incrementCount ind = SWC.incrementWindowCount (mkRideWindowCountKey ind riderId.getId)

getTotalRidesCount :: (HedisFlow m r, CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => Id Person.Person -> m Int
getTotalRidesCount riderId = Redis.withNonCriticalCrossAppRedis $ do
  let key = mkTotalRidesKey riderId.getId
  mbTotalCount <- Redis.safeGet key
  case mbTotalCount of
    Just totalCount -> pure totalCount
    Nothing -> do
      totalCount <- B.runInReplica $ QB.findCountByRiderIdAndStatus riderId BT.COMPLETED
      -- totalCount <- QB.findCountByRiderIdAndStatus riderId BT.COMPLETED
      Redis.setExp key totalCount 14400
      pure totalCount

getRidesCountInWindow ::
  (HedisFlow m r, CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) =>
  Id Person.Person ->
  Int ->
  Int ->
  UTCTime ->
  m Int
getRidesCountInWindow riderId start window currTime =
  Redis.withNonCriticalCrossAppRedis $ do
    let startTime = addUTCTime (fromIntegral (- start)) currTime
        endTime = addUTCTime (fromIntegral window) startTime
    B.runInReplica $ QB.findCountByRideIdStatusAndTime riderId BT.COMPLETED startTime endTime

anyFraudDetected :: (CacheFlow m r, MonadFlow m, EsqDBFlow m r, EsqDBReplicaFlow m r) => Id Person.Person -> Id DMOC.MerchantOperatingCity -> [DMC.MerchantConfig] -> m (Maybe DMC.MerchantConfig)
anyFraudDetected riderId merchantOperatingCityId = checkFraudDetected riderId merchantOperatingCityId [MoreCancelling, MoreCancelledByDriver, MoreSearching, TotalRides, TotalRidesInWindow]

checkFraudDetected :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => Id Person.Person -> Id DMOC.MerchantOperatingCity -> [Factors] -> [DMC.MerchantConfig] -> m (Maybe DMC.MerchantConfig)
checkFraudDetected riderId merchantOperatingCityId factors merchantConfigs = Redis.withNonCriticalCrossAppRedis $ do
  useFraudDetection <- maybe False (.useFraudDetection) <$> CMSUC.findByMerchantOperatingCityId merchantOperatingCityId
  if useFraudDetection
    then findM (\mc -> and <$> mapM (getFactorResult mc) factors) merchantConfigs
    else pure Nothing
  where
    getFactorResult mc factor =
      case factor of
        MoreCancelling -> do
          cancelledBookingCount :: Int <- sum . catMaybes <$> SWC.getCurrentWindowValues (mkCancellationKey mc.id.getId riderId.getId) mc.fraudBookingCancellationCountWindow
          pure $ cancelledBookingCount >= mc.fraudBookingCancellationCountThreshold
        MoreCancelledByDriver -> do
          cancelledBookingByDriverCount :: Int <- sum . catMaybes <$> SWC.getCurrentWindowValues (mkCancellationByDriverKey mc.id.getId riderId.getId) mc.fraudBookingCancelledByDriverCountWindow
          pure $ cancelledBookingByDriverCount >= mc.fraudBookingCancelledByDriverCountThreshold
        MoreSearching -> do
          searchCount :: Int <- sum . catMaybes <$> SWC.getCurrentWindowValues (mkSearchCounterKey mc.id.getId riderId.getId) mc.fraudSearchCountWindow
          pure $ searchCount >= mc.fraudSearchCountThreshold
        TotalRides -> do
          totalRidesCount :: Int <- getTotalRidesCount riderId
          pure $ totalRidesCount <= mc.fraudBookingTotalCountThreshold
        TotalRidesInWindow -> do
          windowValueList <- SWC.getCurrentWindowValues (mkRideWindowCountKey mc.id.getId riderId.getId) mc.fraudRideCountWindow
          let timeInterval = SWC.convertPeriodTypeToSeconds mc.fraudRideCountWindow.periodType
          let intervals = [0, (fromIntegral timeInterval) .. ((length windowValueList -1) * fromIntegral timeInterval)]
          currTime <- getCurrentTime
          let roundedTime = SWC.incrementPeriod mc.fraudRideCountWindow.periodType currTime
              actualTime = addUTCTime (- fromIntegral timeInterval) roundedTime
              swo = mc.fraudRideCountWindow
              keyList = SWC.getkeysForLastPeriods swo actualTime $ SWC.makeSlidingWindowKey mc.fraudRideCountWindow.periodType (mkRideWindowCountKey mc.id.getId riderId.getId)
              list = zip3 windowValueList intervals keyList
          rideCount <-
            mapM
              ( \(key, value, windowKey) -> case key of
                  Just res -> return res
                  Nothing -> do
                    rideCount <- getRidesCountInWindow riderId value (fromIntegral timeInterval) actualTime
                    Redis.setExp windowKey rideCount 86400
                    return rideCount
              )
              list

          let totalRideCount = sum rideCount
          return $ totalRideCount <= mc.fraudRideCountThreshold

blockCustomer :: (CacheFlow m r, MonadFlow m, EsqDBFlow m r) => Id Person.Person -> Maybe (Id DMC.MerchantConfig) -> m ()
blockCustomer riderId mcId = do
  regTokens <- RT.findAllByPersonId riderId
  for_ regTokens $ \regToken -> do
    let key = authTokenCacheKey regToken.token
    void $ Redis.del key
  -- runNoTransaction $ do
  _ <- RT.deleteByPersonId riderId
  void $ QP.updatingEnabledAndBlockedState riderId mcId True
