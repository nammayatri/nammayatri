{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.SpecialZoneDriverDemand
  ( mkSpecialZoneQueueRequestLockKey,
    mkGateSearchDemandKey,
    mkGateSearchSupplyKey,
    mkGateDriverNotifiedKey,
    mkQueueSkipCountKey,
    incrementGateSearchDemand,
    decrementGateSearchDemand,
    runDemandDecrementForBooking,
    incrementGateSearchSupply,
    decrementGateSearchSupply,
    runSupplyIncrementForRequest,
    runSupplyDecrementForRequest,
    incrementQueueSkipCount,
    resetQueueSkipCount,
    checkAndNotifyDriverDemand,
    runDemandCheckForVariants,
    forceNotifyDriverDemand,
    handleQueueSkipIfApplicable,
    completePickupZoneRequestsForDriver,
    cancelPickupZoneRequestsForDriver,
  )
where

import Data.List (sortOn)
import qualified Data.Map as Map
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.SpecialZoneQueueRequest as DSZQR
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

mkSpecialZoneQueueRequestLockKey :: Text -> Text
mkSpecialZoneQueueRequestLockKey requestId = "SpecialZoneQueueRequest:Lock:" <> requestId

mkGateSearchDemandKey :: Text -> Text -> Text
mkGateSearchDemandKey gateId variant = "DriverDemand:Gate:" <> gateId <> ":" <> variant

mkGateSearchSupplyKey :: Text -> Text -> Text
mkGateSearchSupplyKey gateId variant = "DriverSupply:Gate:" <> gateId <> ":" <> variant

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
  Int -> -- TTL in seconds
  m ()
incrementGateSearchDemand gateId variant ttlInSec = Redis.withCrossAppRedis $ do
  let key = mkGateSearchDemandKey gateId variant
  void $ Redis.incr key
  Redis.expire key ttlInSec

-- | Decrement the gate demand counter (clamp at 0). Called when demand is fulfilled or retracted.
decrementGateSearchDemand ::
  ( Redis.HedisFlow m r,
    MonadFlow m
  ) =>
  Text -> -- gateId
  Text -> -- vehicleVariant
  m ()
decrementGateSearchDemand gateId variant = Redis.withCrossAppRedis $ do
  let key = mkGateSearchDemandKey gateId variant
  newVal <- Redis.decr key
  when (newVal < 0) $ void $ Redis.set key (0 :: Int)

-- | Idempotent demand decrement keyed by bookingId. Prevents double-decrement on retries.
--   No-op when pickupGateId is Nothing.
runDemandDecrementForBooking ::
  ( Redis.HedisFlow m r,
    MonadFlow m
  ) =>
  Text -> -- bookingId (idempotency key)
  Maybe Text -> -- pickupGateId
  Text -> -- vehicleServiceTier as Text
  m ()
runDemandDecrementForBooking _ Nothing _ = pure ()
runDemandDecrementForBooking bookingId (Just gateId) variant = do
  let idempotencyKey = "DriverDemand:Decremented:" <> bookingId
  wasSet <- Redis.withCrossAppRedis $ Redis.setNxExpire idempotencyKey 86400 ("1" :: Text)
  when wasSet $ decrementGateSearchDemand gateId variant

-- | Increment the gate supply counter. Supply reflects drivers who have accepted a
--   pickup-zone request at this gate/variant and are committed to the pickup.
--   24-hour sliding TTL guards against orphaned counters if explicit decrements are missed.
incrementGateSearchSupply ::
  ( Redis.HedisFlow m r,
    MonadFlow m
  ) =>
  Text -> -- gateId
  Text -> -- vehicleVariant
  m ()
incrementGateSearchSupply gateId variant = Redis.withCrossAppRedis $ do
  let key = mkGateSearchSupplyKey gateId variant
  void $ Redis.incr key
  Redis.expire key 86400

-- | Decrement the gate supply counter (clamp at 0).
decrementGateSearchSupply ::
  ( Redis.HedisFlow m r,
    MonadFlow m
  ) =>
  Text -> -- gateId
  Text -> -- vehicleVariant
  m ()
decrementGateSearchSupply gateId variant = Redis.withCrossAppRedis $ do
  let key = mkGateSearchSupplyKey gateId variant
  newVal <- Redis.decr key
  when (newVal < 0) $ void $ Redis.set key (0 :: Int)

-- | Idempotent supply increment keyed by pickup-zone requestId. Called when a driver accepts.
runSupplyIncrementForRequest ::
  ( Redis.HedisFlow m r,
    MonadFlow m
  ) =>
  Text -> -- requestId (idempotency key)
  Text -> -- gateId
  Text -> -- vehicleVariant
  m ()
runSupplyIncrementForRequest requestId gateId variant = do
  let idempotencyKey = "DriverSupply:Incremented:" <> requestId
  wasSet <- Redis.withCrossAppRedis $ Redis.setNxExpire idempotencyKey 86400 ("1" :: Text)
  when wasSet $ incrementGateSearchSupply gateId variant

-- | Idempotent supply decrement keyed by pickup-zone requestId. Safe to call from any
--   code path that terminates the commitment (cancel / no-show / booking confirm / ride start
--   / driver ride cancel) — only the first call decrements the counter.
runSupplyDecrementForRequest ::
  ( Redis.HedisFlow m r,
    MonadFlow m
  ) =>
  Text -> -- requestId (idempotency key)
  Text -> -- gateId
  Text -> -- vehicleVariant
  m ()
runSupplyDecrementForRequest requestId gateId variant = do
  let idempotencyKey = "DriverSupply:Decremented:" <> requestId
  wasSet <- Redis.withCrossAppRedis $ Redis.setNxExpire idempotencyKey 86400 ("1" :: Text)
  when wasSet $ decrementGateSearchSupply gateId variant

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
      let demandTtl = fromMaybe 300 gate.demandTtlInSec
      incrementGateSearchDemand pickupZoneGateId variant demandTtl
      checkAndNotifyDriverDemand merchantOpCityId merchantId gate variant

-- | Per-variant demand check. Triggered from Select once an estimate is chosen.
--   Compares per-variant demand against the gate's demand threshold; when it's hit and
--   committed supply (tracked via Redis) is below 'min', notifies top LTS-queue drivers
--   up to 'max - supply'.
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
  logDebug $
    "checkAndNotifyDriverDemand gateId=" <> gateId <> " variant=" <> variant
      <> " demand="
      <> show demandCount
      <> " demandThreshold="
      <> show demandThresholdVal
  if demandCount < demandThresholdVal
    then logDebug $ "Demand below threshold, skipping notification gateId=" <> gateId <> " variant=" <> variant
    else do
      let minThreshold = fromMaybe 0 (DGI.minDriverThresholdFor gate variant)
          maxThreshold = fromMaybe minThreshold (DGI.maxDriverThresholdFor gate variant)
      mbSupplyCount <- Redis.withCrossAppRedis $ Redis.get @Int (mkGateSearchSupplyKey gateId variant)
      let supplyCount = fromMaybe 0 mbSupplyCount
      logDebug $
        "checkAndNotifyDriverDemand gateId=" <> gateId <> " variant=" <> variant
          <> " supply="
          <> show supplyCount
          <> " minThreshold="
          <> show minThreshold
          <> " maxThreshold="
          <> show maxThreshold
      if supplyCount >= minThreshold
        then logDebug $ "Supply already at/above minThreshold, skipping gateId=" <> gateId <> " variant=" <> variant
        else do
          let needed = max 0 (maxThreshold - supplyCount)
          when (needed > 0) $ do
            let cooldown = fromMaybe 900 gate.notificationCooldownInSec
            queueResp <- LTSFlow.getQueueDrivers specialLocationId variant
            let sortedDrivers = sortOn (.queuePosition) queueResp.drivers
                queueDriverIds = map (.driverId) sortedDrivers
            logInfo $
              "Notifying drivers gateId=" <> gateId <> " variant=" <> variant
                <> " needed="
                <> show needed
                <> " queueSize="
                <> show (length queueDriverIds)
                <> " cooldown="
                <> show cooldown
            eligible <- filterEligibleDrivers gate specialLocationId variant merchantId gateId queueDriverIds
            logInfo $
              "Eligible drivers after filter gateId=" <> gateId <> " variant=" <> variant
                <> " eligible="
                <> show (length eligible)
                <> " toNotify="
                <> show (min needed (length eligible))
            void $ notifyDrivers merchantOpCityId merchantId gate specialLocationId variant cooldown (take needed eligible)

-- Force notify (dashboard trigger) — notifies priority drivers first, then fills
-- remaining slots from LTS queue order. Skips demand/supply threshold checks.
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
  Maybe [Id DP.Person] -> -- optional priority driver IDs to notify first
  m Int -- returns count of drivers actually notified
forceNotifyDriverDemand merchantOpCityId merchantId gate vehicleType needed mbPriorityDriverIds = do
  let specialLocationId = gate.specialLocationId.getId
      gateId = gate.id.getId
      cooldown = fromMaybe 900 gate.notificationCooldownInSec
      priorityDriverIds = fromMaybe [] mbPriorityDriverIds
  -- Filter priority drivers through eligibility checks (cooldown, skip count, accepted state)
  eligiblePriority <- filterEligibleDrivers gate specialLocationId vehicleType merchantId gateId priorityDriverIds
  priorityCount <- notifyDrivers merchantOpCityId merchantId gate specialLocationId vehicleType cooldown eligiblePriority
  let remaining = max 0 (needed - priorityCount)
  -- Fill remaining from LTS queue
  queueCount <-
    if remaining > 0
      then do
        queueResp <- LTSFlow.getQueueDrivers specialLocationId vehicleType
        let sortedDrivers = sortOn (.queuePosition) queueResp.drivers
            -- Exclude priority drivers already processed
            queueDriverIds = filter (`notElem` priorityDriverIds) $ map (.driverId) sortedDrivers
        eligible <- filterEligibleDrivers gate specialLocationId vehicleType merchantId gateId queueDriverIds
        notifyDrivers merchantOpCityId merchantId gate specialLocationId vehicleType cooldown (take remaining eligible)
      else pure 0
  pure (priorityCount + queueCount)

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
                      arrivalDeadlineTime = Nothing,
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
            -- Skip drivers who already committed to a pickup-zone request; re-notifying
            -- them wouldn't change supply and would just spam the driver. At most one
            -- Accepted request per driver by business rule.
            hasAccepted <- not . null <$> QSZQR.findActiveByDriverId driverId DSZQR.Accepted
            if hasAccepted
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

-- | Booking is progressing (Confirm or StartRide fired) for a driver: decrement demand
--   for the booking's gate/variant and, if the driver had an Accepted pickup-zone request,
--   mark it Completed and decrement supply. A driver has at most one Accepted request at
--   a time. Idempotent:
--     - demand: bookingId-keyed SETNX (runDemandDecrementForBooking)
--     - supply: requestId-keyed SETNX + DB status transition Accepted → Completed
--   Safe to call from both Confirm and StartRide: first fire wins, second is a no-op.
completePickupZoneRequestsForDriver ::
  ( MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r,
    Redis.HedisFlow m r
  ) =>
  Id DP.Person ->
  Text -> -- bookingId (for demand-decrement idempotency)
  Maybe Text -> -- booking.pickupGateId
  Text -> -- booking.vehicleServiceTier (variant)
  m ()
completePickupZoneRequestsForDriver driverId bookingId mbPickupGateId variant = do
  runDemandDecrementForBooking bookingId mbPickupGateId variant
  -- Look up the last request the driver accepted — not just those still in
  -- 'Accepted' status. The CheckPickupZoneArrival job moves arrived drivers
  -- to status=Expired while keeping response=Accept; without this change those
  -- rows would never be marked Completed and supply would leak.
  mbReq <- QSZQR.findLastAcceptedByDriverId driverId
  forM_ mbReq $ \req -> do
    QSZQR.updateResponse (Just DSZQR.Accept) DSZQR.Completed req.id
    runSupplyDecrementForRequest req.id.getId req.gateId req.vehicleType
    logInfo $ "Completed pickup zone request " <> req.id.getId <> " for driver " <> driverId.getId

-- | Driver cancelled the ride that was to fulfill the pickup-zone commitment. Supply
--   retracts — demand is untouched (customer still wants a ride). A driver has at most
--   one Accepted request at a time.
cancelPickupZoneRequestsForDriver ::
  ( MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r,
    Redis.HedisFlow m r
  ) =>
  Id DP.Person ->
  m ()
cancelPickupZoneRequestsForDriver driverId = do
  mbReq <- QSZQR.findLastAcceptedByDriverId driverId
  forM_ mbReq $ \req -> do
    let idempotencyKey = "PickupZoneCancel:Done:" <> req.id.getId
    wasSet <- Redis.withCrossAppRedis $ Redis.setNxExpire idempotencyKey 86400 ("1" :: Text)
    when wasSet $
      unless (req.status == DSZQR.Completed) $ do
        QSZQR.updateResponse (Just DSZQR.Cancelled) DSZQR.Expired req.id
        runSupplyDecrementForRequest req.id.getId req.gateId req.vehicleType
        logInfo $ "Cancelled pickup zone request " <> req.id.getId <> " after ride cancel by driver " <> driverId.getId

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
  Text -> -- searchTryId for idempotency
  m ()
handleQueueSkipIfApplicable Nothing _ _ _ _ = pure ()
handleQueueSkipIfApplicable (Just gateId) vehicleType driverId merchantId searchTryId = do
  -- Idempotency: each searchTry should only increment skip count once, even if
  -- both the allocator timeout and driver reject paths fire.
  let idempotencyKey = "QueueSkip:Done:" <> searchTryId
  wasSet <- Redis.withCrossAppRedis $ Redis.setNxExpire idempotencyKey 86400 ("1" :: Text)
  when wasSet $ do
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
