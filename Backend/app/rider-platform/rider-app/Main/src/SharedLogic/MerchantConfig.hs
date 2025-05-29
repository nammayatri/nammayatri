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
    getRidesCountInWindow,
    updateCustomerAuthCounters,
    checkAuthFraud,
    customerAuthBlock,
  )
where

import Data.Foldable.Extra
import qualified Domain.Types.Booking as BT
import qualified Domain.Types.MerchantConfig as DMC
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as Person
import qualified Domain.Types.SearchRequest as DSR
import Kernel.Prelude
import Kernel.Storage.Clickhouse.Config
import Kernel.Storage.Esqueleto hiding (isNothing)
import Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Kernel.Utils.SlidingWindowCounters as SWC
import qualified Storage.CachedQueries.Merchant.MerchantServiceUsageConfig as CMSUC
import qualified Storage.Clickhouse.Booking as CHB
import qualified Storage.Clickhouse.Person as CHP
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.RegistrationToken as RT
import Tools.Auth (authTokenCacheKey)

data Factors = MoreCancelling | MoreSearching | MoreCancelledByDriver | TotalRides | TotalRidesInWindow

mkCancellationKey :: Text -> Text -> Text
mkCancellationKey ind idtxt = "Customer:CancellationCount:" <> idtxt <> ":" <> ind

mkCancellationByDriverKey :: Text -> Text -> Text
mkCancellationByDriverKey ind idtxt = "Customer:CancellationByDriverCount:" <> idtxt <> ":" <> ind

mkSearchCounterKey :: Text -> Text -> Text
mkSearchCounterKey ind idtxt = "Customer:SearchCounter:" <> idtxt <> ":" <> ind

mkRideWindowCountKey :: Text -> Text -> Text
mkRideWindowCountKey ind idtxt = "Customer:RidesCount:" <> idtxt <> ":" <> ind

mkAuthCounterKey :: Text -> Text -> Text
mkAuthCounterKey ind idtxt = "Customer:AuthCount: " <> idtxt <> ":" <> ind

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

updateCustomerAuthCounters :: (CacheFlow m r, MonadFlow m) => Id Person.Person -> [DMC.MerchantConfig] -> m ()
updateCustomerAuthCounters riderId merchantConfigs = Redis.withNonCriticalCrossAppRedis $ do
  mapM_ (\mc -> whenJust mc.fraudAuthCountWindow $ \window -> incrementCount mc.id.getId window) merchantConfigs
  where
    incrementCount ind = SWC.incrementWindowCount (mkAuthCounterKey ind riderId.getId)

updateTotalRidesCounters :: (CacheFlow m r, MonadFlow m, EsqDBFlow m r, CacheFlow m r, EsqDBReplicaFlow m r, ClickhouseFlow m r) => Person.Person -> m ()
updateTotalRidesCounters rider = do
  totalRidesCount <- getTotalRidesCountForEndRide rider
  whenJust totalRidesCount $ \count' -> do
    QP.updateTotalRidesCount rider.id (Just (count' + 1))

updateTotalRidesInWindowCounters :: (CacheFlow m r, MonadFlow m) => Id Person.Person -> [DMC.MerchantConfig] -> m ()
updateTotalRidesInWindowCounters riderId merchantConfigs = Redis.withNonCriticalCrossAppRedis $ do
  mapM_ (\mc -> incrementCount mc.id.getId mc.fraudRideCountWindow) merchantConfigs
  where
    incrementCount ind = SWC.incrementWindowCount (mkRideWindowCountKey ind riderId.getId)

getTotalRidesCountForEndRide :: (CacheFlow m r, MonadFlow m, EsqDBFlow m r, EsqDBReplicaFlow m r, ClickhouseFlow m r) => Person.Person -> m (Maybe Int)
getTotalRidesCountForEndRide rider
  | Just totalRidesCount <- rider.totalRidesCount = pure (Just totalRidesCount)
  | otherwise = do
    totalCount <- CHP.findTotalRidesCountByPersonId rider.id
    maybe (pure Nothing) (pure $ CHB.findCountByRiderIdAndStatus rider.id BT.COMPLETED rider.createdAt) totalCount

getRidesCountInWindow ::
  (HedisFlow m r, CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, ClickhouseFlow m r) =>
  Id Person.Person ->
  Int ->
  Int ->
  UTCTime ->
  m Int
getRidesCountInWindow riderId start window currTime =
  Redis.withNonCriticalCrossAppRedis $ do
    let startTime = addUTCTime (fromIntegral (- start)) currTime
        endTime = addUTCTime (fromIntegral window) startTime
    CHB.findCountByRideIdStatusAndTime riderId BT.COMPLETED startTime endTime

anyFraudDetected :: (CacheFlow m r, MonadFlow m, EsqDBFlow m r, EsqDBReplicaFlow m r, ClickhouseFlow m r) => Id Person.Person -> Id DMOC.MerchantOperatingCity -> [DMC.MerchantConfig] -> Maybe DSR.SearchRequest -> m (Maybe DMC.MerchantConfig)
anyFraudDetected riderId merchantOperatingCityId mSearchReq = checkFraudDetected riderId merchantOperatingCityId [MoreCancelling, MoreCancelledByDriver, MoreSearching, TotalRides, TotalRidesInWindow] mSearchReq

checkFraudDetected :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, ClickhouseFlow m r) => Id Person.Person -> Id DMOC.MerchantOperatingCity -> [Factors] -> [DMC.MerchantConfig] -> Maybe DSR.SearchRequest -> m (Maybe DMC.MerchantConfig)
checkFraudDetected riderId merchantOperatingCityId factors merchantConfigs mSearchReq = Redis.withNonCriticalCrossAppRedis $ do
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
        TotalRides -> pure $ maybe False (\count' -> count' <= mc.fraudBookingTotalCountThreshold && count' > 0) (mSearchReq >>= DSR.totalRidesCount)
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
              ( \(key, _, _) -> case key of
                  Just res -> return res
                  Nothing -> return 0
              )
              list

          let totalRideCount = sum rideCount
          return $ totalRideCount <= mc.fraudRideCountThreshold

checkAuthFraud :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, ClickhouseFlow m r) => [DMC.MerchantConfig] -> Id Person.Person -> m (Bool, Maybe (Id DMC.MerchantConfig))
checkAuthFraud mc riderId = Redis.withNonCriticalCrossAppRedis $ do
  let configsWithThreshold = filter (\mc' -> isJust mc'.fraudAuthCountThreshold) mc

  results <- forM configsWithThreshold $ \selectedMc -> do
    fraudDetected <- case selectedMc.fraudAuthCountWindow of
      Nothing -> pure False
      Just window -> do
        authCount :: Int <- sum . catMaybes <$> SWC.getCurrentWindowValues (mkAuthCounterKey selectedMc.id.getId riderId.getId) window
        let threshold = selectedMc.fraudAuthCountThreshold
        let isOverThreshold = maybe False (authCount >=) threshold

        when isOverThreshold $ do
          logInfo $ "Auth fraud detected for user " <> riderId.getId <> " with count " <> show authCount
          SWC.deleteCurrentWindowValues (mkAuthCounterKey selectedMc.id.getId riderId.getId) window

        pure isOverThreshold

    pure (fraudDetected, if fraudDetected then Just selectedMc.id else Nothing)

  let authFraudDetected = any fst results
  let fraudMerchantConfigId = listToMaybe [mcId | (True, Just mcId) <- results]

  pure (authFraudDetected, fraudMerchantConfigId)

blockCustomer :: (CacheFlow m r, MonadFlow m, EsqDBFlow m r) => Id Person.Person -> Maybe (Id DMC.MerchantConfig) -> Maybe Person.BlockSource -> Maybe Text -> m ()
blockCustomer riderId mcId blockSource reason = do
  regTokens <- RT.findAllByPersonId riderId
  for_ regTokens $ \regToken -> do
    let key = authTokenCacheKey regToken.token
    void $ Redis.del key
  _ <- RT.deleteByPersonId riderId
  void $ QP.updatingEnabledAndBlockedStateData riderId mcId True blockSource reason

customerAuthBlock :: (CacheFlow m r, MonadFlow m, EsqDBFlow m r) => Id Person.Person -> Maybe (Id DMC.MerchantConfig) -> Maybe Minutes -> m ()
customerAuthBlock riderId mcId blockDurationMinutes = do
  regTokens <- RT.findAllByPersonId riderId
  for_ regTokens $ \regToken -> do
    let key = authTokenCacheKey regToken.token
    void $ Redis.del key

  blockedUntil <- case blockDurationMinutes of
    Nothing -> pure Nothing
    Just mins -> do
      now <- getCurrentTime
      pure $ Just $ addUTCTime (fromIntegral (mins * 60)) now
  _ <- RT.deleteByPersonId riderId
  void $ QP.updatingAuthEnabledAndBlockedState riderId mcId (Just True) blockedUntil
