{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.SpecialZoneDriverDemand
  ( mkGateSearchDemandKey,
    mkGateDriverNotifiedKey,
    mkQueueSkipCountKey,
    incrementGateSearchDemand,
    incrementQueueSkipCount,
    resetQueueSkipCount,
    checkAndNotifyDriverDemand,
    forceNotifyDriverDemand,
    handleQueueSkipIfApplicable,
  )
where

import Data.List (sortOn)
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.SpecialZoneQueueRequest as DSZQR
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Queries.GateInfo as QGI
import qualified Lib.Queries.SpecialLocation as QSL
import qualified Lib.Types.GateInfo as DGI
import qualified SharedLogic.External.LocationTrackingService.Flow as LTSFlow
import SharedLogic.External.LocationTrackingService.Types (HasLocationService)
import qualified Storage.Queries.SpecialZoneQueueRequest as QSZQR
import qualified Tools.Notifications as Notify

-- Redis keys

mkGateSearchDemandKey :: Text -> Text
mkGateSearchDemandKey gateId = "DriverDemand:Gate:" <> gateId

mkGateDriverNotifiedKey :: Text -> Text -> Text
mkGateDriverNotifiedKey gateId driverId = "DriverDemand:Notified:" <> gateId <> ":" <> driverId

mkQueueSkipCountKey :: Text -> Text -> Text
mkQueueSkipCountKey specialLocationId driverId = "DriverDemand:QueueSkip:" <> specialLocationId <> ":" <> driverId

-- Redis operations

incrementGateSearchDemand ::
  ( Redis.HedisFlow m r,
    MonadFlow m
  ) =>
  Text ->
  m ()
incrementGateSearchDemand gateId = Redis.withCrossAppRedis $ do
  let key = mkGateSearchDemandKey gateId
  void $ Redis.incr key
  Redis.expire key 300 -- 5 min sliding window

incrementQueueSkipCount ::
  ( Redis.HedisFlow m r,
    MonadFlow m
  ) =>
  Text ->
  Id DP.Person ->
  Int ->
  m Int
incrementQueueSkipCount specialLocationId driverId ttlInSec = Redis.withCrossAppRedis $ do
  let key = mkQueueSkipCountKey specialLocationId driverId.getId
  newCount <- Redis.incr key
  when (newCount == 1) $ Redis.expire key ttlInSec
  pure $ fromIntegral newCount

resetQueueSkipCount ::
  ( Redis.HedisFlow m r,
    MonadFlow m
  ) =>
  Text ->
  Id DP.Person ->
  m ()
resetQueueSkipCount specialLocationId driverId = Redis.withCrossAppRedis $ do
  let key = mkQueueSkipCountKey specialLocationId driverId.getId
  void $ Redis.del key

-- Demand check and notification

checkAndNotifyDriverDemand ::
  ( Redis.HedisFlow m r,
    MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r,
    Esq.EsqDBReplicaFlow m r,
    HasLocationService m r,
    HasShortDurationRetryCfg r c,
    HasRequestId r
  ) =>
  Id DMOC.MerchantOperatingCity ->
  Id DM.Merchant ->
  DGI.GateInfo ->
  m ()
checkAndNotifyDriverDemand merchantOpCityId merchantId gate = do
  let gateId = gate.id.getId
      specialLocationId = gate.specialLocationId.getId
  -- 1. Check demand
  mbDemandCount <- Redis.withCrossAppRedis $ Redis.get @Int (mkGateSearchDemandKey gateId)
  let demandCount = fromMaybe 0 mbDemandCount
      demandThresholdVal = fromMaybe 2 gate.demandThreshold
  when (demandCount >= demandThresholdVal) $ do
    let minThreshold = fromMaybe 0 gate.minDriverThreshold
    -- 2. Count drivers in pickup zone via LTS nearBy
    driversNearGate <- LTSFlow.nearBy gate.point.lat gate.point.lon (Just False) Nothing 500 merchantId Nothing Nothing
    driversInPickupZone <- filterM (isInsideGateGeometry gate.id) driversNearGate
    let pickupZoneCount = length driversInPickupZone
    when (pickupZoneCount < minThreshold) $ do
      let needed = minThreshold - pickupZoneCount
          cooldown = fromMaybe 900 gate.notificationCooldownInSec
      -- 3. Find parking drivers (nearBy since we don't have vehicleType in search context)
      driversInBigZone <- LTSFlow.nearBy gate.point.lat gate.point.lon (Just False) Nothing 2000 merchantId Nothing Nothing
      let pickupZoneDriverIds = map (.driverId) driversInPickupZone
          parkingDrivers = filter (\d -> d.driverId `notElem` pickupZoneDriverIds) driversInBigZone
      eligible <- filterM (notRecentlyNotified gateId . (.driverId)) parkingDrivers
      let toNotify = take needed eligible
      void $ notifyDrivers merchantOpCityId merchantId gate specialLocationId "" cooldown (map (.driverId) toNotify)
  where
    isInsideGateGeometry gateInfoId driverLoc = do
      mbGate <- Esq.runInReplica $ QGI.findGateInfoIfDriverInsideGatePickupZone (LatLong driverLoc.lat driverLoc.lon)
      pure $ case mbGate of
        Just g -> g.id == gateInfoId
        Nothing -> False

-- Force notify (dashboard trigger) — uses LTS queue order, skips demand/supply threshold checks
forceNotifyDriverDemand ::
  ( Redis.HedisFlow m r,
    MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r,
    Esq.EsqDBReplicaFlow m r,
    HasLocationService m r,
    HasShortDurationRetryCfg r c,
    HasRequestId r
  ) =>
  Id DMOC.MerchantOperatingCity ->
  Id DM.Merchant ->
  DGI.GateInfo ->
  Text -> -- vehicleType
  Int -> -- number of drivers to notify
  m Int -- returns count of drivers actually notified
forceNotifyDriverDemand merchantOpCityId merchantId gate vehicleType needed = do
  let specialLocationId = gate.specialLocationId.getId
      gateId = gate.id.getId
      cooldown = fromMaybe 900 gate.notificationCooldownInSec
  -- Get drivers from LTS queue sorted by queue position
  queueResp <- LTSFlow.getQueueDrivers specialLocationId vehicleType
  let sortedDrivers = sortOn (.queuePosition) queueResp.drivers
      queueDriverIds = map (.driverId) sortedDrivers
  eligible <- filterM (notRecentlyNotified gateId) queueDriverIds
  let toNotify = take needed eligible
  notifyDrivers merchantOpCityId merchantId gate specialLocationId vehicleType cooldown toNotify

-- Common notification logic: create SpecialZoneQueueRequest entries and send FCM
notifyDrivers ::
  ( Redis.HedisFlow m r,
    MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r,
    Esq.EsqDBReplicaFlow m r
  ) =>
  Id DMOC.MerchantOperatingCity ->
  Id DM.Merchant ->
  DGI.GateInfo ->
  Text -> -- specialLocationId
  Text -> -- vehicleType
  Int -> -- cooldown in seconds
  [Id DP.Person] -> -- drivers to notify
  m Int
notifyDrivers merchantOpCityId merchantId gate specialLocationId vehicleType cooldown driverIds = do
  let gateId = gate.id.getId
  mbSpecialLocation <- Esq.runInReplica $ QSL.findById (Id specialLocationId)
  specialLocationName <- case mbSpecialLocation of
    Just sl -> pure sl.locationName
    Nothing -> do
      logWarning $ "SpecialLocation not found for id: " <> specialLocationId <> ", using gate name as fallback"
      pure gate.name
  now <- getCurrentTime
  let validTill = addUTCTime 30 now
  foldM
    ( \count driverId -> do
        existingRequests <- QSZQR.findActiveByDriverId driverId DSZQR.Active
        let hasActiveRequest = any (\r -> r.validTill > now) existingRequests
        if hasActiveRequest
          then pure count
          else do
            reqId <- generateGUID
            let request =
                  DSZQR.SpecialZoneQueueRequest
                    { id = reqId,
                      driverId = driverId,
                      gateId = gateId,
                      specialLocationId = specialLocationId,
                      merchantId = merchantId,
                      merchantOperatingCityId = merchantOpCityId,
                      status = DSZQR.Active,
                      response = Nothing,
                      validTill = validTill,
                      gateName = gate.name,
                      specialLocationName = specialLocationName,
                      vehicleType = vehicleType,
                      createdAt = now,
                      updatedAt = now
                    }
            QSZQR.create request
            Notify.notifyPickupZoneRequest merchantOpCityId driverId reqId gate.name specialLocationName validTill
            Redis.withCrossAppRedis $
              Redis.setExp (mkGateDriverNotifiedKey gateId driverId.getId) ("1" :: Text) cooldown
            logInfo $ "Notified driver " <> driverId.getId <> " to move to pickup zone at gate " <> gate.name
            pure (count + 1)
    )
    (0 :: Int)
    driverIds

notRecentlyNotified ::
  ( Redis.HedisFlow m r,
    MonadFlow m
  ) =>
  Text ->
  Id DP.Person ->
  m Bool
notRecentlyNotified gId driverId = do
  mbVal <- Redis.withCrossAppRedis $ Redis.get @Text (mkGateDriverNotifiedKey gId driverId.getId)
  pure $ isNothing mbVal

-- Queue skip handling

handleQueueSkipIfApplicable ::
  ( Redis.HedisFlow m r,
    MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r,
    Esq.EsqDBReplicaFlow m r,
    HasLocationService m r,
    HasShortDurationRetryCfg r c,
    HasRequestId r
  ) =>
  Maybe Text -> -- pickupZoneGateId from SearchRequest
  Text -> -- vehicleType for LTS queue
  Id DP.Person ->
  Id DM.Merchant ->
  m ()
handleQueueSkipIfApplicable Nothing _ _ _ = pure ()
handleQueueSkipIfApplicable (Just gateId) vehicleType driverId merchantId = do
  mbGateInfo <- Esq.runInReplica $ QGI.findById (Id gateId)
  case mbGateInfo >>= (.maxRideSkipsBeforeQueueRemoval) of
    Nothing -> pure ()
    Just threshold -> do
      let slId = maybe "" (.getId) ((.specialLocationId) <$> mbGateInfo)
      newCount <- incrementQueueSkipCount slId driverId 86400 -- 24hr TTL
      when (newCount >= threshold) $ do
        void $ LTSFlow.manualQueueRemove slId vehicleType merchantId driverId
        resetQueueSkipCount slId driverId
        logInfo $ "Driver " <> driverId.getId <> " removed from queue at " <> slId <> " after " <> show newCount <> " skips"
