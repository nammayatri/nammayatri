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
    runDemandCheckForVariants,
    forceNotifyDriverDemand,
    handleQueueSkipIfApplicable,
    completePickupZoneRequestOnRideStart,
  )
where

import Data.List (sortOn)
import qualified Data.Map as Map
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
import qualified Storage.Queries.Vehicle as QVehicle
import qualified Tools.Notifications as Notify

-- Redis keys

mkGateSearchDemandKey :: Text -> Text -> Text
mkGateSearchDemandKey gateId variant = "DriverDemand:Gate:" <> gateId <> ":" <> variant

mkGateDriverNotifiedKey :: Text -> Text -> Text
mkGateDriverNotifiedKey gateId driverId = "DriverDemand:Notified:" <> gateId <> ":" <> driverId

mkQueueSkipCountKey :: Text -> Text -> Text
mkQueueSkipCountKey specialLocationId driverId = "DriverDemand:QueueSkip:" <> specialLocationId <> ":" <> driverId

-- Redis operations

incrementGateSearchDemand ::
  ( Redis.HedisFlow m r,
    MonadFlow m
  ) =>
  Text -> -- gateId
  Text -> -- vehicleVariant
  m ()
incrementGateSearchDemand gateId variant = Redis.withCrossAppRedis $ do
  let key = mkGateSearchDemandKey gateId variant
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

-- | Self-contained pickup-zone demand pipeline meant to be called inside a
--   single 'fork' from the Select handler. Bundles:
--     1. gate lookup by id
--     2. per-variant demand-counter increment
--     3. per-variant 'checkAndNotifyDriverDemand'
--   Returns immediately if the gate is not found. Independent of the main
--   ride-booking flow — failures here must never affect Select.
runDemandCheckForVariants ::
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
  Text -> -- pickupZoneGateId from SearchRequest
  [Text] -> -- chosen vehicle variants (de-duplicated by caller)
  m ()
runDemandCheckForVariants merchantOpCityId merchantId pickupZoneGateId variants = do
  mbGate <- Esq.runInReplica $ QGI.findById (Id pickupZoneGateId)
  case mbGate of
    Nothing -> logWarning $ "runDemandCheckForVariants: gate not found id=" <> pickupZoneGateId
    Just gate -> forM_ variants $ \variant -> do
      incrementGateSearchDemand pickupZoneGateId variant
      checkAndNotifyDriverDemand merchantOpCityId merchantId gate variant

-- | Per-variant demand check. Triggered from Select once an estimate is chosen.
--   Notifies parking-area drivers of this variant to move to the pickup zone
--   when supply is below 'min' for the gate, capped at 'max - currentInZone'.
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
  Text -> -- vehicleVariant (service tier)
  m ()
checkAndNotifyDriverDemand merchantOpCityId merchantId gate variant = do
  let gateId = gate.id.getId
      specialLocationId = gate.specialLocationId.getId
  mbDemandCount <- Redis.withCrossAppRedis $ Redis.get @Int (mkGateSearchDemandKey gateId variant)
  let demandCount = fromMaybe 0 mbDemandCount
      demandThresholdVal = fromMaybe 2 (DGI.demandThresholdFor gate variant)
  when (demandCount >= demandThresholdVal) $ do
    let minThreshold = fromMaybe 0 (DGI.minDriverThresholdFor gate variant)
        -- If max isn't configured, fall back to min so we never overshoot the trigger threshold.
        maxThreshold = fromMaybe minThreshold (DGI.maxDriverThresholdFor gate variant)
    -- Single source of truth: LTS queue for this gate's special location, filtered by variant.
    queueResp <- LTSFlow.getQueueDrivers specialLocationId variant
    let sortedDrivers = sortOn (.queuePosition) queueResp.drivers
        queueDriverIds = map (.driverId) sortedDrivers
    (insidePickupZone, parkingDrivers) <- partitionInsideGate gate queueDriverIds
    let pickupZoneCount = length insidePickupZone
    when (pickupZoneCount < minThreshold) $ do
      let needed = max 0 (maxThreshold - pickupZoneCount)
      when (needed > 0) $ do
        let cooldown = fromMaybe 900 gate.notificationCooldownInSec
        eligible <- filterEligibleDrivers gate specialLocationId variant merchantId gateId parkingDrivers
        void $ notifyDrivers merchantOpCityId merchantId gate specialLocationId variant cooldown (take needed eligible)

-- | Split a list of driver ids into (insidePickupZone, outside) based on driver locations.
--   Batch-fetches locations once; falls back to "outside" for drivers with unknown location.
partitionInsideGate ::
  ( MonadFlow m,
    HasLocationService m r,
    HasShortDurationRetryCfg r c,
    HasRequestId r,
    CacheFlow m r,
    EsqDBFlow m r,
    Esq.EsqDBReplicaFlow m r
  ) =>
  DGI.GateInfo ->
  [Id DP.Person] ->
  m ([Id DP.Person], [Id DP.Person])
partitionInsideGate _ [] = pure ([], [])
partitionInsideGate gate driverIds = do
  locations <- LTSFlow.driversLocation driverIds
  let locById = Map.fromList $ map (\l -> (l.driverId, l)) locations
  insideFlags <- forM driverIds $ \dId ->
    case Map.lookup dId locById of
      Nothing -> pure (dId, False)
      Just loc -> do
        mbDriverGate <- Esq.runInReplica $ QGI.findGateInfoIfDriverInsideGatePickupZone (LatLong loc.lat loc.lon)
        pure (dId, maybe False (\g -> g.id == gate.id) mbDriverGate)
  let inside = [d | (d, True) <- insideFlags]
      outside = [d | (d, False) <- insideFlags]
  pure (inside, outside)

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
  eligible <- filterEligibleDrivers gate specialLocationId vehicleType merchantId gateId queueDriverIds
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
  let responseTimeoutSec = fromMaybe 15 gate.pickupRequestResponseTimeoutInSec
      validTill = addUTCTime (fromIntegral responseTimeoutSec) now
  -- Bulk fetch vehicle variants for all drivers
  vehicles <- QVehicle.findAllByDriverIds driverIds
  let vehicleVariantMap = Map.fromList $ map (\v -> (v.driverId, show v.variant)) vehicles
  foldM
    ( \count driverId -> do
        activeRequests <- QSZQR.findActiveByDriverId driverId DSZQR.Active
        acceptedRequests <- QSZQR.findActiveByDriverId driverId DSZQR.Accepted
        let hasActiveRequest = any (\r -> r.validTill > now) activeRequests || not (null acceptedRequests)
        if hasActiveRequest
          then pure count
          else do
            reqId <- generateGUID
            let driverVehicleType = fromMaybe vehicleType (Map.lookup driverId vehicleVariantMap)
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
                      vehicleType = driverVehicleType,
                      createdAt = now,
                      updatedAt = now
                    }
            QSZQR.create request
            let entityData =
                  Notify.PickupZoneRequestEntityData
                    { requestId = reqId.getId,
                      gateName = gate.name,
                      gateAddress = gate.address,
                      specialLocationName = specialLocationName,
                      specialLocationId = specialLocationId,
                      gateId = gateId,
                      vehicleType = driverVehicleType,
                      validTill = validTill,
                      requestType = "PICKUP_ZONE_REQUEST"
                    }
            Notify.notifyPickupZoneRequest merchantOpCityId driverId entityData
            Redis.withCrossAppRedis $
              Redis.setExp (mkGateDriverNotifiedKey gateId driverId.getId) ("1" :: Text) cooldown
            logInfo $ "Notified driver " <> driverId.getId <> " to move to pickup zone at gate " <> gate.name
            pure (count + 1)
    )
    (0 :: Int)
    driverIds

-- Filter eligible drivers: not recently notified + increment skip count + remove if threshold exceeded
filterEligibleDrivers ::
  ( Redis.HedisFlow m r,
    MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r,
    Esq.EsqDBReplicaFlow m r,
    HasLocationService m r,
    HasShortDurationRetryCfg r c,
    HasRequestId r
  ) =>
  DGI.GateInfo ->
  Text -> -- specialLocationId
  Text -> -- vehicleType
  Id DM.Merchant ->
  Text -> -- gateId
  [Id DP.Person] ->
  m [Id DP.Person]
filterEligibleDrivers gate specialLocationId vehicleType merchantId gateId driverIds = do
  let maxSkips = gate.maxRideSkipsBeforeQueueRemoval
  foldM
    ( \acc driverId -> do
        recentlyNotified <- not <$> notRecentlyNotified gateId driverId
        if recentlyNotified
          then pure acc
          else do
            -- Increment skip count for every request we're about to send
            shouldInclude <- case maxSkips of
              Nothing -> pure True
              Just threshold -> do
                newCount <- incrementQueueSkipCount specialLocationId driverId 86400
                if newCount >= threshold
                  then do
                    void $ LTSFlow.manualQueueRemove specialLocationId vehicleType merchantId driverId
                    resetQueueSkipCount specialLocationId driverId
                    logInfo $ "Driver " <> driverId.getId <> " removed from queue after " <> show newCount <> " requests at gate " <> gateId
                    pure False
                  else pure True
            pure $ if shouldInclude then acc ++ [driverId] else acc
    )
    []
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

-- Mark Accepted pickup zone request as Completed when driver starts a ride
completePickupZoneRequestOnRideStart ::
  ( MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id DP.Person ->
  m ()
completePickupZoneRequestOnRideStart driverId = do
  acceptedRequests <- QSZQR.findActiveByDriverId driverId DSZQR.Accepted
  forM_ acceptedRequests $ \req -> do
    QSZQR.updateResponse (Just DSZQR.Accept) DSZQR.Completed req.id
    logInfo $ "Marked pickup zone request " <> req.id.getId <> " as Completed for driver " <> driverId.getId

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
