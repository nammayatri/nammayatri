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
    incrementSkipAndCheckThreshold,
    checkAndNotifyDriverDemand,
    runDemandCheckForVariants,
    forceNotifyDriverDemand,
    handleQueueSkipIfApplicable,
    completePickupZoneRequestsForDriver,
    cancelPickupZoneRequestsForDriver,
    releasePickupZoneCountersOnCancel,
    filterByGateProximity,
    clearAirportPerKmFareCacheForPolicy,
    getAirportPerKmFare,
  )
where

import Control.Monad.Extra (mapMaybeM)
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT
import Data.List (partition, sortOn)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import qualified Domain.Types.Common as DTC
import qualified Domain.Types.FarePolicy as DFP
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.ServiceTierType as DVST
import qualified Domain.Types.SpecialZoneQueueRequest as DSZQR
import qualified Domain.Types.VehicleVariant as DV
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Storage.InMem as IM
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Id
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Kernel.Utils.Common
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Queries.GateInfo as QGI
import qualified Lib.Queries.SpecialLocation as QSL
import qualified Lib.Types.GateInfo as DGI
import qualified Lib.Types.SpecialLocation as SL
import qualified SharedLogic.External.LocationTrackingService.Flow as LTSFlow
import SharedLogic.External.LocationTrackingService.Types (HasLocationService)
import qualified SharedLogic.FareCalculator as SFC
import qualified SharedLogic.FareProduct as SharedFareProduct
import qualified SharedLogic.Merchant as SMerchant
import qualified Storage.Cac.FarePolicy as CQFP
import qualified Storage.Cac.TransporterConfig as CTC
import qualified Storage.CachedQueries.VehicleServiceTier as CQVST
import qualified Storage.Queries.DriverInformationExtra as QDI
import qualified Storage.Queries.FareProduct as QFareProduct
import qualified Storage.Queries.SpecialZoneQueueRequest as QSZQR
import qualified Storage.Queries.VehicleExtra as QV
import qualified Tools.Notifications as Notify

-- | Drivers within this many meters of a *different* queueable gate at the same
--   special location are presumed physically committed to that gate (e.g. a
--   driver standing at T1 should not get a T2 pickup request).
gateProximityExclusionMeters :: Double
gateProximityExclusionMeters = 150.0

-- | Representative airport-ride distance used to derive a single per-km figure
--   from a fare policy. The fare per km isn't constant — base fare amortises over
--   distance, and per-km section rates can change across distance bands — so we
--   evaluate the full fare pipeline at this distance and divide.
--   15 km = typical airport-to-city run.
representativeAirportRideDistanceMeters :: Int
representativeAirportRideDistanceMeters = 15000

-- | Representative ride duration paired with the distance above. Used by per-min
--   congestion charge and ride-extra-time-fare computations inside the fare
--   calculator. 30 min ~ 30 km/h average ~ 15 km.
representativeAirportRideDurationSec :: Int
representativeAirportRideDurationSec = 30 * 60

airportPerKmFareCacheTtlSec :: Int
airportPerKmFareCacheTtlSec = 86400 -- 1 day

mkAirportPerKmFareCacheKey :: Id SL.SpecialLocation -> DV.VehicleVariant -> Text
mkAirportPerKmFareCacheKey slId variant =
  "AirportPerKmFare:" <> slId.getId <> ":" <> T.pack (show variant)

getAirportPerKmFare ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    Esq.EsqDBReplicaFlow m r,
    BeamFlow m r,
    MonadFlow m
  ) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Id SL.SpecialLocation ->
  LatLong ->
  Text -> -- pickupGateId of the gate we're notifying for
  DVST.ServiceTierType -> -- driver's vehicle service tier
  m (Maybe HighPrecMoney)
getAirportPerKmFare merchantId merchantOpCityId specialLocationId gateLatLong pickupGateId serviceTier = do
  let driverVariant = DV.castServiceTierToVariant serviceTier
      cacheKey = mkAirportPerKmFareCacheKey specialLocationId driverVariant
  mbCached <- Redis.withCrossAppRedis $ Redis.safeGet @(Maybe HighPrecMoney) cacheKey
  case mbCached of
    Just cached -> pure cached
    Nothing -> do
      mbFare <- computeAirportPerKmFare merchantId merchantOpCityId gateLatLong pickupGateId driverVariant
      Redis.withCrossAppRedis $ Redis.setExp cacheKey mbFare airportPerKmFareCacheTtlSec
      pure mbFare

clearAirportPerKmFareCacheForPolicy ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m
  ) =>
  Id DFP.FarePolicy ->
  m ()
clearAirportPerKmFareCacheForPolicy fpId = do
  fareProducts <- QFareProduct.findAllFareProductByFarePolicyId fpId
  let pickupProducts =
        [ (fp.merchantOperatingCityId, fp.vehicleServiceTier, slId)
          | fp <- fareProducts,
            SL.Pickup slId _ <- [fp.area]
        ]
      byCity =
        Map.fromListWith (<>) [(cityId, [(tier, slId)]) | (cityId, tier, slId) <- pickupProducts]
  cacheKeys <-
    fmap concat . forM (Map.toList byCity) $ \(cityId, tierSlPairs) -> do
      cityServiceTiers <- CQVST.findAllByMerchantOpCityId cityId Nothing Nothing
      let variantsForTier tier =
            [ v
              | vst <- cityServiceTiers,
                vst.serviceTierType == tier,
                v <- vst.defaultForVehicleVariant
            ]
      pure
        [ mkAirportPerKmFareCacheKey slId v
          | (tier, slId) <- tierSlPairs,
            v <- variantsForTier tier
        ]
  Redis.runInMultiCloudRedisWrite $ Redis.withCrossAppRedis $ forM_ cacheKeys Redis.del

computeAirportPerKmFare ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    Esq.EsqDBReplicaFlow m r,
    BeamFlow m r,
    MonadFlow m
  ) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  LatLong ->
  Text ->
  DV.VehicleVariant ->
  m (Maybe HighPrecMoney)
computeAirportPerKmFare merchantId merchantOpCityId gateLatLong pickupGateId driverVariant = do
  cityServiceTiers <- CQVST.findAllByMerchantOpCityId merchantOpCityId Nothing Nothing
  let eligibleServiceTiers =
        [ vst.serviceTierType
          | vst <- cityServiceTiers,
            driverVariant `elem` vst.defaultForVehicleVariant
        ]
  if null eligibleServiceTiers
    then do
      logError $ "No eligible service tiers found for driver variant " <> show driverVariant <> " in merchantOpCityId " <> show merchantOpCityId
      pure Nothing
    else do
      let searchSources = SharedFareProduct.getSearchSources False
          preferenceOrder =
            [ DTC.OneWay DTC.OneWayRideOtp,
              DTC.OneWay DTC.OneWayOnDemandStaticOffer,
              DTC.OneWay DTC.OneWayOnDemandDynamicOffer
            ]
          tryCategory eligibleTier tripCategory = do
            res <- SharedFareProduct.getAllFareProducts merchantId merchantOpCityId searchSources gateLatLong Nothing Nothing Nothing tripCategory
            pure $ find (\fp -> fp.vehicleServiceTier == eligibleTier) res.fareProducts
      fareProducts <- mapMaybeM (\eligibleTier -> firstJustM (tryCategory eligibleTier) preferenceOrder) eligibleServiceTiers
      if null fareProducts
        then pure Nothing
        else do
          possibleFares <-
            mapMaybeM
              ( \fp -> do
                  mbFarePolicy <- CQFP.findById Nothing fp.farePolicyId
                  case mbFarePolicy of
                    Nothing -> pure Nothing
                    Just farePolicy -> Just <$> computePerKmFromPolicy fp farePolicy
              )
              fareProducts
          if null possibleFares
            then pure Nothing
            else pure . Just $ maximum possibleFares
  where
    firstJustM _ [] = pure Nothing
    firstJustM f (x : xs) = do
      r <- f x
      maybe (firstJustM f xs) (pure . Just) r

    staticCongestionDetails =
      DFP.CongestionChargeDetails
        { dpVersion = Just "Static",
          mbSupplyDemandRatioToLoc = Nothing,
          mbSupplyDemandRatioFromLoc = Nothing,
          congestionChargePerMin = Nothing,
          smartTipSuggestion = Nothing,
          smartTipReason = Nothing,
          mbActualQARFromLocGeohash = Nothing,
          mbActualQARCity = Nothing
        }
    computePerKmFromPolicy fp farePolicy = do
      let fullFarePolicy =
            DFP.farePolicyToFullFarePolicy
              merchantId
              fp.vehicleServiceTier
              fp.tripCategory
              Nothing -- cancellationFarePolicy: not relevant for per-km
              staticCongestionDetails
              Nothing -- no congestionChargeData
              farePolicy
              fp.disableRecompute
          representativeMeters = Meters representativeAirportRideDistanceMeters
          representativeDuration = Seconds representativeAirportRideDurationSec
      currency <- SMerchant.getCurrencyByMerchantOpCity merchantOpCityId
      distanceUnit <- SMerchant.getDistanceUnitByMerchantOpCity merchantOpCityId
      mbTransporterConfig <- CTC.findByMerchantOpCityId merchantOpCityId Nothing
      now <- getCurrentTime
      fareParams <-
        SFC.calculateFareParameters
          SFC.CalculateFareParametersParams
            { farePolicy = fullFarePolicy,
              actualDistance = Just representativeMeters,
              rideTime = now,
              returnTime = Nothing,
              roundTrip = False,
              waitingTime = Nothing,
              stopWaitingTimes = [],
              actualRideDuration = Nothing,
              vehicleAge = Nothing,
              driverSelectedFare = Nothing,
              customerExtraFee = Nothing,
              petCharges = Nothing,
              nightShiftCharge = Nothing,
              estimatedCongestionCharge = Nothing,
              customerCancellationDues = Nothing,
              nightShiftOverlapChecking = False,
              estimatedDistance = Just representativeMeters,
              estimatedRideDuration = Just representativeDuration,
              timeDiffFromUtc = Nothing,
              tollCharges = Nothing,
              currency,
              noOfStops = 0,
              shouldApplyBusinessDiscount = False,
              shouldApplyPersonalDiscount = True,
              distanceUnit,
              merchantOperatingCityId = Just merchantOpCityId,
              mbAdditonalChargeCategories = Nothing,
              numberOfLuggages = Nothing,
              govtChargesRate = mbTransporterConfig <&> (.taxConfig.rideGst),
              pickupGateId = Just pickupGateId
            }
      let estimatedFare = SFC.fareSum fareParams (Just [])
          -- Tolls and gate parking (which now includes airport entry fee added by
          -- applyAirportEntryFee in the V2 path) are flat per-trip reimbursements,
          -- not earnings per km.
          excludedFlat =
            fromMaybe 0 fareParams.tollCharges
              + fromMaybe 0 fareParams.tollFareTax
              + fromMaybe 0 fareParams.parkingCharge
              + fromMaybe 0 fareParams.parkingChargeTax
          variableFare = estimatedFare - excludedFlat
          distanceKm :: HighPrecMoney
          distanceKm = HighPrecMoney (toRational representativeAirportRideDistanceMeters / 1000)
      pure $ if distanceKm > 0 then variableFare / distanceKm else 0

-- Redis keys

mkSpecialZoneQueueRequestLockKey :: Text -> Text
mkSpecialZoneQueueRequestLockKey requestId = "SpecialZoneQueueRequest:Lock:" <> requestId

mkGateSearchDemandKey :: Text -> Text -> Text
mkGateSearchDemandKey gateId variant = "DriverDemand:Gate:" <> gateId <> ":" <> variant

mkGateSearchSupplyKey :: Text -> Text -> Text
mkGateSearchSupplyKey gateId variant = "DriverSupply:Gate:" <> gateId <> ":" <> variant

-- | Cooldown key. Embeds 'Redis.shardHashTag' so a batch of writes for many
--   drivers groups into a small number of cluster slots, letting
--   'Redis.bulkShardedRedisBatch' pipeline efficiently. Shard count is sourced
--   from 'gateNotifiedKeyShards' in AppEnv.
mkGateDriverNotifiedKey :: Int -> Text -> Text -> Text
mkGateDriverNotifiedKey shards gateId driverId =
  "DriverDemand:Notified:" <> gateId <> ":" <> driverId <> ":" <> Redis.shardHashTag shards driverId

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
incrementGateSearchDemand gateId variant ttlInSec = Redis.runInMasterCloudRedisCellWithCrossAppRedis $ do
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
decrementGateSearchDemand gateId variant = Redis.runInMasterCloudRedisCellWithCrossAppRedis $ do
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
  wasSet <- Redis.runInMasterCloudRedisCellWithCrossAppRedis $ Redis.setNxExpire idempotencyKey 86400 ("1" :: Text)
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
incrementGateSearchSupply gateId variant = Redis.runInMasterCloudRedisCellWithCrossAppRedis $ do
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
decrementGateSearchSupply gateId variant = Redis.runInMasterCloudRedisCellWithCrossAppRedis $ do
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
  wasSet <- Redis.runInMasterCloudRedisCellWithCrossAppRedis $ Redis.setNxExpire idempotencyKey 86400 ("1" :: Text)
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
  wasSet <- Redis.runInMasterCloudRedisCellWithCrossAppRedis $ Redis.setNxExpire idempotencyKey 86400 ("1" :: Text)
  when wasSet $ decrementGateSearchSupply gateId variant

incrementQueueSkipCount ::
  ( Redis.HedisFlow m r,
    MonadFlow m
  ) =>
  Text ->
  Id DP.Person ->
  Int ->
  m Int
incrementQueueSkipCount specialLocationId driverId ttlInSec = Redis.runInMasterCloudRedisCellWithCrossAppRedis $ do
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
resetQueueSkipCount specialLocationId driverId = Redis.runInMasterCloudRedisCellWithCrossAppRedis $ do
  let key = mkQueueSkipCountKey specialLocationId driverId.getId
  void $ Redis.del key

-- | Record a true queue skip for a driver: bump the per-(specialLocation, driver)
--   skip counter and, if the gate's removal threshold is reached, kick the driver
--   out of the LTS queue and clear the counter. Threshold = Nothing means "track
--   the count but never auto-remove" (still useful — the driver UI surfaces the
--   running count via the GET endpoint). Caller owns the threshold lookup so we
--   don't refetch the gate per call.
incrementSkipAndCheckThreshold ::
  ( Redis.HedisFlow m r,
    MonadFlow m,
    HasLocationService m r,
    HasShortDurationRetryCfg r c,
    HasRequestId r
  ) =>
  Text -> -- specialLocationId
  Text -> -- vehicleType
  Id DM.Merchant ->
  Id DP.Person ->
  Maybe Int -> -- threshold; Nothing = no auto-removal
  m ()
incrementSkipAndCheckThreshold specialLocationId vehicleType merchantId driverId mbThreshold = do
  newCount <- incrementQueueSkipCount specialLocationId driverId 86400
  whenJust mbThreshold $ \threshold ->
    when (newCount >= threshold) $ do
      logError $
        "[incrementSkipAndCheckThreshold] manualQueueRemove triggered: driverId=" <> driverId.getId
          <> ", specialLocationId="
          <> specialLocationId
          <> ", vehicleType="
          <> vehicleType
          <> ", skipCount="
          <> show newCount
          <> ", threshold="
          <> show threshold
          <> ", reason=skip-count threshold reached"
      void $ LTSFlow.manualQueueRemove specialLocationId vehicleType merchantId driverId (Just "skip_threshold_reached")
      resetQueueSkipCount specialLocationId driverId
      logInfo $ "Driver " <> driverId.getId <> " removed from queue after " <> show newCount <> " skips at " <> specialLocationId

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
    ServiceFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    Esq.EsqDBReplicaFlow m r,
    BeamFlow m r,
    HasLocationService m r,
    HasShortDurationRetryCfg r c,
    HasRequestId r,
    HasFlowEnv m r '["maxNotificationShards" ::: Int],
    Redis.HedisLTSFlowEnv r,
    HasField "gateNotifiedKeyShards" r Int
  ) =>
  Id DMOC.MerchantOperatingCity ->
  Id DM.Merchant ->
  Text -> -- bookingId (idempotency key, symmetric with runDemandDecrementForBooking)
  Text -> -- pickupZoneGateId from SearchRequest
  [(Text, DVST.ServiceTierType)] -> -- (vehicleType key for queue/Redis, serviceTier for fare lookup)
  m ()
runDemandCheckForVariants merchantOpCityId merchantId bookingId pickupZoneGateId variants = do
  mbGate <- QGI.findById (Id pickupZoneGateId)
  case mbGate of
    Nothing -> logWarning $ "runDemandCheckForVariants: gate not found id=" <> pickupZoneGateId
    Just gate -> do
      mbSpecialLocation <- QSL.findById gate.specialLocationId
      let isQueueEnabled = fromMaybe False (mbSpecialLocation >>= (.isQueueEnabled))
      unless isQueueEnabled $
        logDebug $ "runDemandCheckForVariants: queue not enabled for specialLocation=" <> gate.specialLocationId.getId <> ", skipping"
      when isQueueEnabled $
        forM_ variants $ \(variant, serviceTier) -> do
          -- Idempotency key mirrors runDemandDecrementForBooking so retried Init calls
          -- (Beckn allows them) don't inflate demand vs the single-fire decrement.
          let idempotencyKey = "DriverDemand:Incremented:" <> bookingId <> ":" <> variant
          wasSet <- Redis.withCrossAppRedis $ Redis.setNxExpire idempotencyKey 86400 ("1" :: Text)
          when wasSet $ do
            let demandTtl = 86400 -- 1 day
            incrementGateSearchDemand pickupZoneGateId variant demandTtl
          -- Notification check runs every Init (driver pool / cooldown have their own
          -- guards) — we just don't want to double-count demand on retries.
          checkAndNotifyDriverDemand merchantOpCityId merchantId gate variant (Just serviceTier) (Just DSZQR.App)

-- | Per-variant demand check. Triggered from Select once an estimate is chosen.
--   Compares per-variant demand against the gate's demand threshold; when it's hit and
--   committed supply (tracked via Redis) is below 'min', notifies top LTS-queue drivers
--   up to 'max - supply'.
checkAndNotifyDriverDemand ::
  ( MonadFlow m,
    ServiceFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    Esq.EsqDBReplicaFlow m r,
    BeamFlow m r,
    HasLocationService m r,
    HasShortDurationRetryCfg r c,
    HasRequestId r,
    HasFlowEnv m r '["maxNotificationShards" ::: Int],
    Redis.HedisLTSFlowEnv r,
    HasField "gateNotifiedKeyShards" r Int
  ) =>
  Id DMOC.MerchantOperatingCity ->
  Id DM.Merchant ->
  DGI.GateInfo ->
  Text -> -- vehicleVariant (queue/Redis key)
  Maybe DVST.ServiceTierType -> -- service tier for airport per-km fare lookup; Nothing skips fare calc
  Maybe DSZQR.TriggerSource -> -- trigger source threaded through to notifyDrivers
  m ()
checkAndNotifyDriverDemand merchantOpCityId merchantId gate variant mbServiceTier mbTriggerSource = do
  let gateId = gate.id.getId
      specialLocationId = gate.specialLocationId.getId
  mbDemandCount <- Redis.runInMasterCloudRedisCellWithCrossAppRedis $ Redis.get @Int (mkGateSearchDemandKey gateId variant)
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
      mbSupplyCount <- Redis.runInMasterCloudRedisCellWithCrossAppRedis $ Redis.get @Int (mkGateSearchSupplyKey gateId variant)
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
            eligible <- filterEligibleDrivers variant gateId gate.enableQueueFilter queueDriverIds
            eligibleNearGate <- filterByGateProximity gate eligible
            logInfo $
              "Eligible drivers after filter gateId=" <> gateId <> " variant=" <> variant
                <> " eligible="
                <> show (length eligible)
                <> " eligibleNearGate="
                <> show (length eligibleNearGate)
                <> " toNotify="
                <> show (min needed (length eligibleNearGate))
            void $ notifyDrivers merchantOpCityId merchantId gate specialLocationId variant mbServiceTier cooldown mbTriggerSource Nothing (take needed eligibleNearGate)

-- Force notify (dashboard trigger) — notifies priority drivers first, then fills
-- remaining slots from LTS queue order. Skips demand/supply threshold checks.
forceNotifyDriverDemand ::
  ( MonadFlow m,
    ServiceFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    Esq.EsqDBReplicaFlow m r,
    BeamFlow m r,
    HasLocationService m r,
    HasShortDurationRetryCfg r c,
    HasRequestId r,
    HasFlowEnv m r '["maxNotificationShards" ::: Int],
    Redis.HedisLTSFlowEnv r,
    HasField "gateNotifiedKeyShards" r Int
  ) =>
  Id DMOC.MerchantOperatingCity ->
  Id DM.Merchant ->
  DGI.GateInfo ->
  Text -> -- vehicleType
  Int -> -- number of drivers to notify
  Maybe [Id DP.Person] -> -- optional priority driver IDs to notify first
  Maybe Bool -> -- isDemandHigh flag (dashboard-supplied, surfaced to driver app)
  m Int -- returns count of drivers actually notified
forceNotifyDriverDemand merchantOpCityId merchantId gate vehicleType needed mbPriorityDriverIds mbIsDemandHigh = do
  let specialLocationId = gate.specialLocationId.getId
      gateId = gate.id.getId
      cooldown = fromMaybe 900 gate.notificationCooldownInSec
      priorityDriverIds = fromMaybe [] mbPriorityDriverIds
      -- Dashboard is the only caller of this force path; stamp every row it produces.
      triggerSource = Just DSZQR.Dashboard
      -- Dashboard's `vehicleType` is a free-form Text; try to parse it as ServiceTierType
      -- so the airport per-km fare lookup can match the right fare product. If parsing
      -- fails, fare calc is silently skipped (notification still sent without perKmFare).
      mbServiceTier = readMaybe (T.unpack vehicleType) :: Maybe DVST.ServiceTierType
  -- Filter priority drivers through eligibility checks (cooldown, skip count, accepted state)
  eligiblePriority <- filterEligibleDrivers vehicleType gateId gate.enableQueueFilter priorityDriverIds
  eligiblePriorityNearGate <- filterByGateProximity gate eligiblePriority
  priorityCount <- notifyDrivers merchantOpCityId merchantId gate specialLocationId vehicleType mbServiceTier cooldown triggerSource mbIsDemandHigh eligiblePriorityNearGate
  let remaining = max 0 (needed - priorityCount)
  -- Fill remaining from LTS queue
  queueCount <-
    if remaining > 0
      then do
        let mbVariant = DV.castServiceTierToVariant <$> mbServiceTier
        case mbVariant of
          Nothing -> do
            logWarning $ "forceNotifyDriverDemand: unable to parse vehicleType as service tier, skipping fare calc: " <> vehicleType
            pure 0
          Just variant -> do
            queueResp <- LTSFlow.getQueueDrivers specialLocationId (show variant)
            let sortedDrivers = sortOn (.queuePosition) queueResp.drivers
                -- Exclude priority drivers already processed
                queueDriverIds = filter (`notElem` priorityDriverIds) $ map (.driverId) sortedDrivers
            eligible <- filterEligibleDrivers vehicleType gateId gate.enableQueueFilter queueDriverIds
            eligibleNearGate <- filterByGateProximity gate eligible
            notifyDrivers merchantOpCityId merchantId gate specialLocationId vehicleType mbServiceTier cooldown triggerSource mbIsDemandHigh (take remaining eligibleNearGate)
      else pure 0
  pure (priorityCount + queueCount)

-- Common notification logic: create SpecialZoneQueueRequest entries and send FCM + GRPC
notifyDrivers ::
  ( MonadFlow m,
    ServiceFlow m r,
    CacheFlow m r,
    Redis.HedisLTSFlowEnv r,
    EsqDBFlow m r,
    Esq.EsqDBReplicaFlow m r,
    BeamFlow m r,
    HasFlowEnv m r '["maxNotificationShards" ::: Int],
    HasField "gateNotifiedKeyShards" r Int
  ) =>
  Id DMOC.MerchantOperatingCity ->
  Id DM.Merchant ->
  DGI.GateInfo ->
  Text -> -- specialLocationId
  Text -> -- vehicleType
  Maybe DVST.ServiceTierType -> -- service tier for airport per-km fare lookup; Nothing skips fare calc
  Int -> -- cooldown in seconds
  Maybe DSZQR.TriggerSource -> -- trigger source for audit (App | Dashboard)
  Maybe Bool -> -- isDemandHigh override (Nothing => default True)
  [Id DP.Person] -> -- drivers to notify
  m Int
notifyDrivers merchantOpCityId merchantId gate specialLocationId vehicleType mbServiceTier cooldown mbTriggerSource mbIsDemandHigh driverIds = do
  let gateId = gate.id.getId
  mbSpecialLocation <- QSL.findById (Id specialLocationId)
  specialLocationName <- case mbSpecialLocation of
    Just sl -> pure sl.locationName
    Nothing -> do
      logWarning $ "SpecialLocation not found for id: " <> specialLocationId <> ", using gate name as fallback"
      pure gate.name
  now <- getCurrentTime
  let responseTimeoutSec = fromMaybe 15 gate.pickupRequestResponseTimeoutInSec
      validTill = addUTCTime (fromIntegral responseTimeoutSec) now
      notificationDuration = fromMaybe 15 gate.pickupRequestResponseTimeoutInSec
      notificationActiveTillInSec = fromMaybe 30 gate.notificationActiveTillInSec
      notificationValidTill = addUTCTime (fromIntegral notificationActiveTillInSec) now
      isDemandHigh = fromMaybe True mbIsDemandHigh
  mbPerKmFare <- case mbServiceTier of
    Nothing -> pure Nothing
    Just serviceTier -> getAirportPerKmFare merchantId merchantOpCityId (Id specialLocationId) gate.point gateId serviceTier
  mbDemandCount <- Redis.runInMasterCloudRedisCellWithCrossAppRedis $ Redis.get @Int (mkGateSearchDemandKey gateId vehicleType)
  let demandCount = fromMaybe 0 mbDemandCount
  -- Callers funnel through 'filterEligibleDrivers' which has already done the
  -- bulk cooldown / Accepted / Active-pending / on-ride filtering. We trust
  -- the input here and just build + create + notify; cooldown semantics cover
  -- the small race window of two near-concurrent calls.
  -- Build every row upfront so we can do a single bulk insert and then fan the
  -- per-driver notify + cooldown work out in parallel via fork. Previous loop
  -- did 1 INSERT + 1 FCM + 1 GRPC + 1 Redis SET per driver in series.
  toNotify <- forM driverIds $ \driverId -> do
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
              arrivalDeadlineTime = Nothing,
              triggerSource = mbTriggerSource,
              createdAt = now,
              updatedAt = now
            }
    pure (driverId, reqId, request)
  QSZQR.createMany (map (\(_, _, r) -> r) toNotify)
  -- Cooldown markers must land before this function returns: 'forceNotifyDriverDemand'
  -- can call notifyDrivers twice back-to-back, and the second call's eligibility
  -- filter relies on these keys to avoid double-notifying. 'bulkShardedRedisBatch'
  -- groups items by cluster slot (the '{shard-N}' tag baked into
  -- 'mkGateDriverNotifiedKey' funnels keys with the same hash bucket onto one
  -- slot) and forks per shard, bounded by 'clusterMGetForkLimit'. Each shard's
  -- action then issues 'setExpMany' which packages every key in the bucket into
  -- a single atomic Lua-script EVAL — one TCP round-trip per shard.
  shards <- asks (.gateNotifiedKeyShards)
  let cooldownKey driverId = mkGateDriverNotifiedKey shards gateId driverId.getId
  void $
    Redis.bulkShardedRedisBatch
      (\(driverId, _, _) -> cooldownKey driverId)
      ( \items -> do
          Redis.setExpMany
            cooldown
            [(cooldownKey driverId, "1" :: Text) | (driverId, _, _) <- items]
          pure (() <$ items)
      )
      toNotify
  -- FCM + GRPC are external network calls and dominate the per-driver cost; fan
  -- them out in parallel so total wallclock is the slowest call, not the sum.
  forM_ toNotify $ \(driverId, reqId, _) -> do
    let entityData =
          Notify.PickupZoneRequestEntityData
            { requestId = reqId.getId,
              gateName = gate.name,
              gateAddress = gate.address,
              specialLocationName = specialLocationName,
              specialLocationId = specialLocationId,
              gateId = gateId,
              vehicleType = vehicleType,
              validTill = validTill,
              notificationDuration = notificationDuration,
              notificationValidTill = notificationValidTill,
              requestType = "PICKUP_ZONE_REQUEST",
              perKmFare = mbPerKmFare,
              isDemandHigh = isDemandHigh,
              demandCount = demandCount
            }
    fork ("notifyPickupZone-" <> reqId.getId) $ do
      Notify.notifyPickupZoneRequest merchantOpCityId driverId entityData
      logInfo $ "Notified driver " <> driverId.getId <> " to move to pickup zone at gate " <> gate.name
  pure (length toNotify)

-- Filter eligible drivers: not in cooldown, not already committed to a pickup-zone
-- request, and not currently on a ride. Skip-count accounting now lives at the
-- true skip paths (Reject in postSpecialZoneQueueRequestRespond, Ignored expiry
-- in collectValidActive, search-flow timeout in handleQueueSkipIfApplicable) so
-- merely being considered here no longer penalises a driver who may not even be
-- notified after `take needed`.
--
-- Bulk shape: replaces 3N per-driver round-trips with three bulk fetches —
-- cooldown markers via shard-aware MGET, Active-or-Accepted requests via
-- 'findAllByDriverIdsAndStatuses', and driver-information via
-- 'findAllByDriverIds'. Final filter is in-memory and preserves input order.
--
-- Why both Active and Accepted in one query:
--   * Accepted = driver already committed; re-notifying does nothing useful
--     and just spams them.
--   * Active with validTill > now = a pending un-responded notification is
--     out there; sending another would be noise (cooldown also covers most
--     of this, but the Active check tightens the race window between two
--     near-concurrent notify calls).
-- 'notifyDrivers' trusts this filter and skips its own busy-check.
filterEligibleDrivers ::
  ( Redis.HedisFlow m r,
    MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r,
    Esq.EsqDBReplicaFlow m r,
    Forkable m,
    HasField "gateNotifiedKeyShards" r Int
  ) =>
  Text -> -- vehicleType
  Text -> -- gateId
  Maybe (Map.Map Text Bool) -> -- gate.enableQueueFilter: when entry is True, vehicle.canSwitchToAirport must be Just True
  [Id DP.Person] ->
  m [Id DP.Person]
filterEligibleDrivers _ _ _ [] = pure []
filterEligibleDrivers vehicleType gateId mbFilterAirport driverIds = do
  shards <- asks (.gateNotifiedKeyShards)
  now <- getCurrentTime
  let cooldownKeyFor d = mkGateDriverNotifiedKey shards gateId d.getId
      cooldownKeys = map cooldownKeyFor driverIds
  -- One pipelined MGET per cluster shard, shards in parallel. The '{shard-N}'
  -- hash tag in 'mkGateDriverNotifiedKey' funnels keys onto a bounded number
  -- of slots so each shard's batch is meaningful.
  -- Note: NOT inside 'withCrossAppRedis'. This key is private to the special-
  -- zone-notify flow, so it should carry the app's standard key prefix. The
  -- write side (notifyDrivers' 'bulkShardedRedisBatch' + 'setExpMany') is also
  -- un-wrapped, so reader and writer apply the same 'keyModifier' and the
  -- bytes match — wrapping only one side would silently break "find my SET".
  cooldownHits <- Redis.mGetClusterWithKeys @Text cooldownKeys
  let inCooldownKeys = Set.fromList (map fst cooldownHits)
  -- Bulk DB: drivers with an Accepted (committed) or pending Active request.
  busyRows <- QSZQR.findAllByDriverIdsAndStatuses driverIds [DSZQR.Active, DSZQR.Accepted]
  let busyDrivers =
        Set.fromList $
          mapMaybe
            ( \r ->
                if r.status == DSZQR.Accepted || r.validTill > now
                  then Just r.driverId
                  else Nothing
            )
            busyRows
  -- Bulk DB: drivers currently on a ride.
  driverInfos <- QDI.findAllByDriverIds (map (.getId) driverIds)
  let onRideDriversOrOfflineDrivers =
        Set.fromList
          [ info.driverId
            | info <- driverInfos,
              info.onRide || info.mode == Just DTC.OFFLINE
          ]
      airportIneligibleDrivers =
        Set.fromList
          [ info.driverId
            | info <- driverInfos,
              not info.canSwitchToAirport
          ]
  -- Bulk DB: driver vehicles for service-tier and per-vehicle airport-switch filters.
  let mbTier = readMaybe (T.unpack vehicleType) :: Maybe DVST.ServiceTierType
      vehicleAirportFilterEnabled = fromMaybe False (Map.lookup vehicleType =<< mbFilterAirport)
  vehicleEligibleDriverIds <- case mbTier of
    Nothing -> pure driverIds
    Just tier -> do
      vehicles <- QV.findAllByDriverIds driverIds
      let eligibleSet =
            Set.fromList
              [ v.driverId
                | v <- vehicles,
                  tier `elem` v.selectedServiceTiers,
                  not vehicleAirportFilterEnabled || v.enableForAirport == Just True
              ]
      pure $ filter (`Set.member` eligibleSet) driverIds
  pure $
    filter
      ( \d ->
          not (Set.member (cooldownKeyFor d) inCooldownKeys)
            && not (Set.member d busyDrivers)
            && not (Set.member d onRideDriversOrOfflineDrivers)
            && not (Set.member d airportIneligibleDrivers)
      )
      vehicleEligibleDriverIds

-- | Cached snapshot of a queueable gate used for the proximity check. Carries the
--   parsed pickup-zone polygon(s) so the in-memory filter can do point-in-polygon
--   and edge-distance checks without hitting Postgres on the hot path.
--   `polygons` is empty when the gate has no/invalid geom — in that case the
--   filter falls back to a haversine check around `centerPoint`.
data CachedGateForProximity = CachedGateForProximity
  { gateId :: Text,
    canQueueUpOnGate :: Bool,
    centerPoint :: LatLong,
    polygons :: [[[LatLong]]] -- list of polygons; each polygon is rings (outer first, then holes); each ring is closed.
  }
  deriving (Show, Generic, FromJSON, ToJSON, Typeable)

-- | Fetch all gates of a special location with their parsed pickup-zone polygons.
--   1-hour in-memory cache keyed by specialLocationId; gate polygons are admin-edited
--   and rarely change, so a stale read is acceptable.
getCachedGatesForProximity ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    Esq.EsqDBReplicaFlow m r,
    MonadFlow m,
    CoreMetrics m
  ) =>
  Id SL.SpecialLocation ->
  m [CachedGateForProximity]
getCachedGatesForProximity slId =
  IM.withInMemCache ["GateProximityPolygons", slId.getId] 3600 $ do
    rows <- QGI.findAllGatesBySpecialLocationId slId
    pure $
      map
        ( \(g, mbGeoJson) ->
            CachedGateForProximity
              { gateId = g.id.getId,
                canQueueUpOnGate = g.canQueueUpOnGate,
                centerPoint = g.point,
                polygons = fromMaybe [] (mbGeoJson >>= parseGatePolygons)
              }
        )
        rows

-- | Parse PostGIS ST_AsGeoJSON output (Polygon or MultiPolygon) into a list of
--   polygons. Each polygon is a list of closed rings (outer + holes); each ring
--   is a list of LatLong (note GeoJSON stores [lon, lat]).
parseGatePolygons :: Text -> Maybe [[[LatLong]]]
parseGatePolygons txt = do
  v <- either (const Nothing) Just (A.eitherDecodeStrict (TE.encodeUtf8 txt))
  AT.parseMaybe parseShape v
  where
    parseShape :: A.Value -> AT.Parser [[[LatLong]]]
    parseShape = A.withObject "Geometry" $ \o -> do
      typ :: Text <- o A..: "type"
      coords <- o A..: "coordinates"
      case typ of
        "Polygon" -> do
          poly <- parsePolygon coords
          pure [poly]
        "MultiPolygon" -> A.withArray "MultiPolygonCoords" (mapM parsePolygon . V.toList) coords
        _ -> fail $ "Unsupported geometry type: " <> show typ

    parsePolygon = A.withArray "PolygonCoords" $ \rings -> mapM parseRing (V.toList rings)
    parseRing = A.withArray "Ring" $ \pts -> mapM parsePoint (V.toList pts)
    parsePoint = A.withArray "Coord" $ \pair -> do
      coords :: [Double] <- mapM AT.parseJSON (V.toList pair)
      case coords of
        (lon : lat : _) -> pure $ LatLong lat lon
        _ -> fail "Expected [lon, lat] coordinate"

-- | Ray-casting point-in-ring test. Assumes a closed ring (last == first).
pointInRing :: LatLong -> [LatLong] -> Bool
pointInRing _ ring | length ring < 4 = False
pointInRing (LatLong y x) ring =
  foldl' step False (zip ring (drop 1 ring))
  where
    step inside (LatLong y1 x1, LatLong y2 x2)
      | y1 == y2 = inside -- horizontal edge: ignore (avoids div-by-zero)
      | ((y1 > y) /= (y2 > y))
          && (x < (x2 - x1) * (y - y1) / (y2 - y1) + x1) =
        not inside
      | otherwise = inside

-- | Inside the polygon (outer ring AND not in any hole).
pointInPolygon :: LatLong -> [[LatLong]] -> Bool
pointInPolygon _ [] = False
pointInPolygon p (outer : holes) = pointInRing p outer && not (any (pointInRing p) holes)

-- | Approximate point-to-segment distance in meters using equirectangular projection.
--   Fine for radii <<< Earth radius (we use ~150m here).
pointToSegmentMeters :: LatLong -> LatLong -> LatLong -> Double
pointToSegmentMeters p a b =
  let latRad = (a.lat + b.lat) / 2 * pi / 180
      mPerDegLat = 111320.0 :: Double
      mPerDegLon = 111320.0 * cos latRad
      ax = a.lon * mPerDegLon
      ay = a.lat * mPerDegLat
      bx = b.lon * mPerDegLon
      by = b.lat * mPerDegLat
      px = p.lon * mPerDegLon
      py = p.lat * mPerDegLat
      dx = bx - ax
      dy = by - ay
      lenSq = dx * dx + dy * dy
      t = if lenSq <= 0 then 0 else max 0 (min 1 (((px - ax) * dx + (py - ay) * dy) / lenSq))
      cx = ax + t * dx
      cy = ay + t * dy
   in sqrt ((px - cx) * (px - cx) + (py - cy) * (py - cy))

-- | Min distance from point to any edge of any ring of the polygon (meters).
minDistanceToPolygonEdges :: LatLong -> [[LatLong]] -> Double
minDistanceToPolygonEdges _ [] = 1 / 0
minDistanceToPolygonEdges p rings =
  minimum $
    map
      (\ring -> if length ring < 2 then 1 / 0 else minimum (zipWith (pointToSegmentMeters p) ring (drop 1 ring)))
      rings

-- | True iff the driver is inside any of the gate's polygons OR within `radius`
--   meters of any polygon's edge. Falls back to haversine-from-centerPoint when
--   the gate has no parsed polygons.
isPointInOrNearGate :: LatLong -> Double -> CachedGateForProximity -> Bool
isPointInOrNearGate p radius gate
  | null gate.polygons =
    realToFrac (distanceBetweenInMeters p gate.centerPoint) < radius
  | otherwise =
    any (\poly -> pointInPolygon p poly || minDistanceToPolygonEdges p poly < radius) gate.polygons

-- | Exclude drivers physically committed to a *different* queueable gate at the
--   same special location: their location is inside that gate's pickup polygon
--   (or within 'gateProximityExclusionMeters' of its edge) but not inside the
--   target gate's polygon. Uses an in-memory cached, app-side polygon check —
--   no DB hit per driver in steady state.
--   Drivers with no known location are kept in the pool.
filterByGateProximity ::
  ( CoreMetrics m,
    MonadFlow m,
    CacheFlow m r,
    HasLocationService m r,
    HasShortDurationRetryCfg r c,
    HasRequestId r,
    EsqDBFlow m r,
    Esq.EsqDBReplicaFlow m r
  ) =>
  DGI.GateInfo -> -- target gate (where the request will be sent)
  [Id DP.Person] ->
  m [Id DP.Person]
filterByGateProximity _ [] = pure []
filterByGateProximity targetGate driverIds = do
  cachedGates <- getCachedGatesForProximity targetGate.specialLocationId
  let otherQueueableGates = filter (\g -> g.gateId /= targetGate.id.getId && g.canQueueUpOnGate) cachedGates
      mbTargetCached = find (\g -> g.gateId == targetGate.id.getId) cachedGates
  if null otherQueueableGates
    then pure driverIds
    else do
      driverLocations <- LTSFlow.driversLocation driverIds
      stalenessThreshold <-
        case targetGate.merchantOperatingCityId of
          Just mocId ->
            CTC.findByMerchantOpCityId (cast mocId :: Id DMOC.MerchantOperatingCity) Nothing
              <&> maybe
                driverLocationStalenessThresholdSecondsDefault
                (fromMaybe driverLocationStalenessThresholdSecondsDefault . (.driverLocationStalenessThresholdSeconds))
          Nothing -> pure driverLocationStalenessThresholdSecondsDefault
      now <- getCurrentTime
      let isFresh dl = diffUTCTime now dl.coordinatesCalculatedAt <= intToNominalDiffTime stalenessThreshold.getSeconds
          (freshLocations, staleLocations) = partition isFresh driverLocations
      unless (null staleLocations) $
        logInfo $
          "filterByGateProximity dropped stale driver locations targetGate=" <> targetGate.id.getId
            <> " thresholdSec="
            <> show stalenessThreshold.getSeconds
            <> " staleDriverIds="
            <> show (map (.driverId.getId) staleLocations)
      let locByDriver = Map.fromList $ map (\dl -> (dl.driverId, LatLong dl.lat dl.lon)) freshLocations
          (kept, dropped) = partition (driverNotCommittedToOtherGate locByDriver mbTargetCached otherQueueableGates) driverIds
      unless (null dropped) $
        logInfo $
          "filterByGateProximity excluded drivers near other gate targetGate=" <> targetGate.id.getId
            <> " excluded="
            <> show (map (.getId) dropped)
      pure kept
  where
    driverNotCommittedToOtherGate locMap mbTargetCached otherGates driverId =
      case Map.lookup driverId locMap of
        Nothing -> False -- unknown location: don't penalise
        Just loc ->
          let nearTarget = maybe False (isPointInOrNearGate loc gateProximityExclusionMeters) mbTargetCached
              nearOther = any (isPointInOrNearGate loc gateProximityExclusionMeters) otherGates
           in nearTarget || not nearOther

-- Default driver-location staleness threshold (2 minutes) used when the
-- per-merchant-operating-city override is unset.
driverLocationStalenessThresholdSecondsDefault :: Seconds
driverLocationStalenessThresholdSecondsDefault = Seconds 120

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
    wasSet <- Redis.runInMasterCloudRedisCellWithCrossAppRedis $ Redis.setNxExpire idempotencyKey 86400 ("1" :: Text)
    when wasSet $
      unless (req.status == DSZQR.Completed) $ do
        QSZQR.updateResponse (Just DSZQR.Cancelled) DSZQR.Expired req.id
        runSupplyDecrementForRequest req.id.getId req.gateId req.vehicleType
        logInfo $ "Cancelled pickup zone request " <> req.id.getId <> " after ride cancel by driver " <> driverId.getId

-- | Release pickup-zone counters for a cancelled booking. Single entry point used by every
--   cancel path (user/application/merchant/driver) so all four flows stay in lockstep.
--   Both underlying calls are idempotent (bookingId-keyed for demand, requestId-keyed for
--   supply), so safe to invoke from multiple cancel sites for the same booking.
--   demandStaysLive=True for ByDriver: that source triggers reallocation, so the booking's
--   demand is still live for the next driver. All other sources terminate the booking.
releasePickupZoneCountersOnCancel ::
  ( Redis.HedisFlow m r,
    MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Bool -> -- demandStaysLive
  Text -> -- bookingId
  Maybe Text -> -- pickupGateId from booking
  Text -> -- vehicleVariant string
  Maybe (Id DP.Person) -> -- assigned driverId, if any
  m ()
releasePickupZoneCountersOnCancel demandStaysLive bookingId mbPickupGateId variant mbDriverId = do
  unless demandStaysLive $
    runDemandDecrementForBooking bookingId mbPickupGateId variant
  forM_ mbDriverId cancelPickupZoneRequestsForDriver

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
  wasSet <- Redis.runInMasterCloudRedisCellWithCrossAppRedis $ Redis.setNxExpire idempotencyKey 86400 ("1" :: Text)
  when wasSet $ do
    mbGateInfo <- QGI.findById (Id gateId)
    case mbGateInfo >>= (.maxRideSkipsBeforeQueueRemoval) of
      Nothing -> pure ()
      Just threshold -> do
        let slId = maybe "" (.getId) ((.specialLocationId) <$> mbGateInfo)
        newCount <- incrementQueueSkipCount slId driverId 86400 -- 24hr TTL
        when (newCount >= threshold) $ do
          logError $
            "[handleQueueSkipIfApplicable] manualQueueRemove triggered: driverId=" <> driverId.getId
              <> ", specialLocationId="
              <> slId
              <> ", gateId="
              <> gateId
              <> ", vehicleType="
              <> vehicleType
              <> ", searchTryId="
              <> searchTryId
              <> ", skipCount="
              <> show newCount
              <> ", threshold="
              <> show threshold
              <> ", reason=search-flow timeout, skip-count threshold reached"
          void $ LTSFlow.manualQueueRemove slId vehicleType merchantId driverId (Just "search_skip_threshold")
          resetQueueSkipCount slId driverId
          logInfo $ "Driver " <> driverId.getId <> " removed from queue at " <> slId <> " after " <> show newCount <> " skips"
