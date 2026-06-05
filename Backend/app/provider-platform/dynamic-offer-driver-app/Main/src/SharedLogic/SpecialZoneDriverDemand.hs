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
    getCalloutVariantsForTier,
  )
where

import Control.Monad.Extra (mapMaybeM, partitionM)
import Data.List (nub, partition, sortOn)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
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
import Kernel.Utils.Common
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import Lib.GateInfo.Geometry (CachedGateForProximity (..), isPointInOrNearGate, parseGatePolygons)
import qualified Lib.Queries.GateInfo as QGI
import qualified Lib.Queries.SpecialLocation as QSL
import qualified Lib.Types.GateInfoExtra as DGI
import qualified Lib.Types.SpecialLocation as SL
import qualified SharedLogic.External.LocationTrackingService.Flow as LTSFlow
import SharedLogic.External.LocationTrackingService.Types (HasLocationService)
import qualified SharedLogic.FareCalculator as SFC
import qualified SharedLogic.FareProduct as SharedFareProduct
import qualified SharedLogic.Merchant as SMerchant
import Storage.Beam.SpecialZone ()
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

mkAirportPerKmFareCacheKey :: Id SL.SpecialLocation -> DVST.ServiceTierType -> Text
mkAirportPerKmFareCacheKey slId serviceTier =
  "AirportPerKmFare:" <> slId.getId <> ":" <> T.pack (show serviceTier)

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
  let cacheKey = mkAirportPerKmFareCacheKey specialLocationId serviceTier
  mbCached <- Redis.withCrossAppRedis $ Redis.safeGet @(Maybe HighPrecMoney) cacheKey
  case mbCached of
    Just cached -> pure cached
    Nothing -> do
      calloutVariants <- getCalloutVariantsForTier merchantOpCityId (Just specialLocationId.getId) (Just serviceTier) (T.pack $ show serviceTier)
      mbFare <- computeAirportPerKmFare merchantId merchantOpCityId gateLatLong pickupGateId calloutVariants
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
        [ (fp.vehicleServiceTier, slId)
          | fp <- fareProducts,
            SL.Pickup slId _ <- [fp.area]
        ]
      cacheKeys = [mkAirportPerKmFareCacheKey slId tier | (tier, slId) <- pickupProducts]
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
  [DV.VehicleVariant] -> -- callout variants
  m (Maybe HighPrecMoney)
computeAirportPerKmFare merchantId merchantOpCityId gateLatLong pickupGateId calloutVariants = do
  cityServiceTiers <- CQVST.findAllByMerchantOpCityId merchantOpCityId Nothing Nothing
  let eligibleServiceTiers =
        [ vst.serviceTierType
          | vst <- cityServiceTiers,
            let vstCalloutVars =
                  if null vst.specialZoneQueueCalloutVariants
                    then vst.defaultForVehicleVariant
                    else vst.specialZoneQueueCalloutVariants,
            any (`elem` vstCalloutVars) calloutVariants
        ]
  if null eligibleServiceTiers
    then do
      logError $ "No eligible service tiers found for callout variants " <> show calloutVariants <> " in merchantOpCityId " <> show merchantOpCityId
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

-- | Look up the LTS queue callout variants for a service tier from the
--   VehicleServiceTier config. Uses 'specialZoneQueueCalloutVariants' when
--   non-empty; falls back to 'castServiceTierToVariant' for backward compat.
getCalloutVariantsForTier ::
  (CacheFlow m r, EsqDBFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  Maybe Text -> -- specialLocationId (for zone-specific VST override)
  Maybe DVST.ServiceTierType ->
  Text -> -- variant fallback text (parsed as VehicleVariant if serviceTier is Nothing)
  m [DV.VehicleVariant]
getCalloutVariantsForTier merchantOpCityId mbSlId mbServiceTier variantFallback =
  case mbServiceTier of
    Just st -> do
      mbVst <- CQVST.findByServiceTierTypeAndCityId st merchantOpCityId Nothing mbSlId
      let calloutVars = maybe [] (.specialZoneQueueCalloutVariants) mbVst
      if null calloutVars
        then pure [DV.castServiceTierToVariant st]
        else pure calloutVars
    Nothing -> pure $ maybeToList (readMaybe (T.unpack variantFallback))

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
            calloutVariants <- getCalloutVariantsForTier merchantOpCityId (Just specialLocationId) mbServiceTier variant
            logDebug $ "checkAndNotifyDriverDemand calloutVariants=" <> show calloutVariants <> " for variant=" <> variant
            allSortedDrivers <- fmap concat $
              forM calloutVariants $ \cv -> do
                queueResp <- measuringDurationToLog INFO ("checkAndNotifyDriverDemand.getQueueDrivers cv=" <> show cv) $ LTSFlow.getQueueDrivers specialLocationId (show cv)
                pure $ sortOn (.queuePosition) queueResp.drivers
            let queueDriverIds = nub $ map (.driverId) allSortedDrivers
            logInfo $
              "Notifying drivers gateId=" <> gateId <> " variant=" <> variant
                <> " needed="
                <> show needed
                <> " queueSize="
                <> show (length queueDriverIds)
                <> " cooldown="
                <> show cooldown
            eligibleNearGate <- filterInBatches 50 needed specialLocationId variant gateId gate.enableQueueFilter merchantId gate queueDriverIds
            logInfo $
              "Eligible drivers after batched filter gateId=" <> gateId <> " variant=" <> variant
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
  (eligiblePriority, priorityVariantMap) <- filterEligibleDrivers merchantId specialLocationId vehicleType gateId gate.enableQueueFilter priorityDriverIds
  calloutVariants <- getCalloutVariantsForTier merchantOpCityId (Just specialLocationId) mbServiceTier vehicleType
  logDebug $ "forceNotifyDriverDemand calloutVariants=" <> show calloutVariants <> " for vehicleType=" <> vehicleType
  if null calloutVariants
    then do
      logWarning $ "forceNotifyDriverDemand: no callout variants for vehicleType: " <> vehicleType
      pure 0
    else do
      eligiblePriorityNearGate <- filterByGateProximity merchantId gate priorityVariantMap eligiblePriority
      priorityCount <- notifyDrivers merchantOpCityId merchantId gate specialLocationId vehicleType mbServiceTier cooldown triggerSource mbIsDemandHigh eligiblePriorityNearGate
      let remaining = max 0 (needed - priorityCount)
      -- Fill remaining from LTS queue
      queueCount <-
        if remaining > 0
          then do
            allSortedDrivers <- fmap concat $
              forM calloutVariants $ \cv -> do
                queueResp <- measuringDurationToLog INFO ("forceNotifyDriverDemand.getQueueDrivers cv=" <> show cv) $ LTSFlow.getQueueDrivers specialLocationId (show cv)
                pure $ sortOn (.queuePosition) queueResp.drivers
            let queueDriverIds = nub $ filter (`notElem` priorityDriverIds) $ map (.driverId) allSortedDrivers
            eligibleNearGate <- filterInBatches 50 remaining specialLocationId vehicleType gateId gate.enableQueueFilter merchantId gate queueDriverIds
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
notifyDrivers _ _ _ _ _ _ _ _ _ [] = pure 0
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
-- | Process queue drivers in batches of `batchSize`, filtering each batch for
--   eligibility and gate proximity. Stops early once `needed` drivers are collected.
filterInBatches ::
  ( Redis.HedisFlow m r,
    MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r,
    Esq.EsqDBReplicaFlow m r,
    CoreMetrics m,
    HasLocationService m r,
    HasShortDurationRetryCfg r c,
    HasRequestId r,
    Forkable m,
    HasField "gateNotifiedKeyShards" r Int
  ) =>
  Int -> -- batch size
  Int -> -- needed count
  Text ->
  Text -> -- vehicleType
  Text -> -- gateId
  Maybe (Map.Map Text Bool) -> -- gate.enableQueueFilter
  Id DM.Merchant ->
  DGI.GateInfo ->
  [Id DP.Person] -> -- all queue driver IDs
  m [Id DP.Person]
filterInBatches batchSize needed specialLocationId vehicleType gateId mbFilterAirport merchantId gate allDriverIds = do
  let batches = chunksOf batchSize allDriverIds
  go [] batches
  where
    go acc [] = pure acc
    go acc _ | length acc >= needed = pure (take needed acc)
    go acc (batch : rest) = do
      (eligible, variantMap) <- filterEligibleDrivers merchantId specialLocationId vehicleType gateId mbFilterAirport batch
      nearGate <- filterByGateProximity merchantId gate variantMap eligible
      go (acc <> nearGate) rest
    chunksOf _ [] = []
    chunksOf n xs = let (h, t) = splitAt n xs in h : chunksOf n t

filterEligibleDrivers ::
  ( Redis.HedisFlow m r,
    MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r,
    Esq.EsqDBReplicaFlow m r,
    Forkable m,
    CoreMetrics m,
    HasLocationService m r,
    HasShortDurationRetryCfg r c,
    HasRequestId r,
    HasField "gateNotifiedKeyShards" r Int
  ) =>
  Id DM.Merchant -> -- merchant (for manualQueueRemove of on-ride/offline drivers)
  Text -> -- specialLocationId
  Text -> -- vehicleType
  Text -> -- gateId
  Maybe (Map.Map Text Bool) -> -- gate.enableQueueFilter: when entry is True, vehicle.canSwitchToAirport must be Just True
  [Id DP.Person] ->
  m ([Id DP.Person], Map.Map (Id DP.Person) Text) -- (eligible drivers, driverId -> variant map)
filterEligibleDrivers _ _ _ _ _ [] = pure ([], Map.empty)
filterEligibleDrivers merchantId specialLocationId vehicleType gateId mbFilterAirport driverIds = do
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
  cooldownHits <- measuringDurationToLog INFO ("filterEligibleDrivers.mGetCooldown n=" <> show (length driverIds)) $ Redis.mGetClusterWithKeys @Text cooldownKeys
  let inCooldownKeys = Set.fromList (map fst cooldownHits)
  -- Bulk DB: drivers with an Accepted (committed) or pending Active request.
  busyRows <- measuringDurationToLog INFO ("filterEligibleDrivers.findBusyRequests n=" <> show (length driverIds)) $ QSZQR.findAllByDriverIdsAndStatuses driverIds [DSZQR.Active, DSZQR.Accepted]
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
  driverInfos <- measuringDurationToLog INFO ("filterEligibleDrivers.findDriverInfos n=" <> show (length driverIds)) $ QDI.findAllByDriverIds (map (.getId) driverIds)
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
  vehicles <- measuringDurationToLog INFO ("filterEligibleDrivers.findVehicles n=" <> show (length driverIds)) $ QV.findAllByDriverIds driverIds
  let driverVariantMap = Map.fromList [(v.driverId, show v.variant) | v <- vehicles]
      getDriverVehicleVariant did = fromMaybe "" (Map.lookup did driverVariantMap)
      mbTier = readMaybe (T.unpack vehicleType) :: Maybe DVST.ServiceTierType
      vehicleAirportFilterEnabled = fromMaybe False (Map.lookup vehicleType =<< mbFilterAirport)
  let vehicleEligibleDriverIds = case mbTier of
        Nothing -> driverIds
        Just tier ->
          let eligibleSet =
                Set.fromList
                  [ v.driverId
                    | v <- vehicles,
                      tier `elem` v.selectedServiceTiers,
                      not vehicleAirportFilterEnabled || v.enableForAirport == Just True
                  ]
           in filter (`Set.member` eligibleSet) driverIds
      eligibleDrivers =
        filter
          ( \d ->
              not (Set.member (cooldownKeyFor d) inCooldownKeys)
                && not (Set.member d busyDrivers)
                && not (Set.member d onRideDriversOrOfflineDrivers)
                && not (Set.member d airportIneligibleDrivers)
          )
          vehicleEligibleDriverIds
      -- Drivers with no business sitting in the special-zone queue — on a ride,
      -- offline, or not airport-eligible — are evicted in the background. Cooldown /
      -- pending-Active drivers are only *temporarily* ineligible and stay in place.
      -- Scoped to this batch's input set so we never touch drivers we didn't fetch
      -- info for; dedup by driverId (on-ride/offline reason wins) so each driver is
      -- removed at most once even when it matches both conditions.
      reasonForQueueRemoval d
        | Set.member d onRideDriversOrOfflineDrivers = Just ("on_ride_or_offline" :: Text)
        | Set.member d airportIneligibleDrivers = Just "cannot_switch_to_airport"
        | otherwise = Nothing
      toRemoveWithReason = mapMaybe (\d -> (,) d <$> reasonForQueueRemoval d) driverIds
  unless (null toRemoveWithReason) $ do
    logInfo $ "filterEligibleDrivers removing ineligible drivers from queue specialLocationId=" <> specialLocationId <> " count=" <> show (length toRemoveWithReason)
    fork "removeIneligibleFromQueue" $
      forM_ toRemoveWithReason $ \(driver, reason) ->
        void $
          LTSFlow.manualQueueRemove
            specialLocationId
            (getDriverVehicleVariant driver)
            merchantId
            driver
            (Just reason)
  pure (eligibleDrivers, driverVariantMap)

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
  Id DM.Merchant -> -- merchant (for manualQueueRemove of drivers LTS has no location for)
  DGI.GateInfo -> -- target gate (where the request will be sent)
  Map.Map (Id DP.Person) Text -> -- driverId -> vehicleVariant map (from filterEligibleDrivers)
  [Id DP.Person] ->
  m [Id DP.Person]
filterByGateProximity _ _ _ [] = pure []
filterByGateProximity merchantId targetGate driverVariantMap driverIds = do
  let getDriverVehicleVariant did = fromMaybe "" (Map.lookup did driverVariantMap)
  cachedGates <- measuringDurationToLog INFO "filterByGateProximity.getCachedGates" $ getCachedGatesForProximity targetGate.specialLocationId
  let otherQueueableGates = filter (\g -> g.gateId /= targetGate.id.getId && g.canQueueUpOnGate) cachedGates
      mbTargetCached = find (\g -> g.gateId == targetGate.id.getId) cachedGates
  if null otherQueueableGates
    then pure driverIds
    else do
      driverLocations <- measuringDurationToLog INFO ("filterByGateProximity.driversLocation n=" <> show (length driverIds)) $ LTSFlow.driversLocation driverIds
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
          locatedDriverIds = Set.fromList (map (.driverId) driverLocations)
      unless (null staleLocations) $
        logInfo $
          "filterByGateProximity dropped stale driver locations targetGate=" <> targetGate.id.getId
            <> " thresholdSec="
            <> show stalenessThreshold.getSeconds
            <> " staleDriverIds="
            <> show (map (.driverId.getId) staleLocations)
      forM_ staleLocations $ \dl ->
        fork ("manualQueueRemoveStale-" <> dl.driverId.getId) $
          void $
            LTSFlow.manualQueueRemove
              targetGate.specialLocationId.getId
              (getDriverVehicleVariant dl.driverId)
              dl.merchantId
              dl.driverId
              (Just "stale_driver_location")
      let locByDriver = Map.fromList $ map (\dl -> (dl.driverId, LatLong dl.lat dl.lon)) freshLocations
      (kept, dropped) <-
        flip partitionM driverIds $ \driverId ->
          case Map.lookup driverId locByDriver of
            Nothing -> do
              unless (Set.member driverId locatedDriverIds) $
                fork ("manualQueueRemoveNoLoc-" <> driverId.getId) $
                  void $
                    LTSFlow.manualQueueRemove
                      targetGate.specialLocationId.getId
                      (getDriverVehicleVariant driverId)
                      merchantId
                      driverId
                      (Just "no_driver_location")
              pure False
            Just loc -> do
              let nearTarget = maybe False (isPointInOrNearGate loc gateProximityExclusionMeters) mbTargetCached
                  nearOther = any (isPointInOrNearGate loc gateProximityExclusionMeters) otherQueueableGates
              pure $ nearTarget || not nearOther
      unless (null dropped) $
        logInfo $
          "filterByGateProximity excluded drivers near other gate targetGate=" <> targetGate.id.getId
            <> " excluded="
            <> show (map (.getId) dropped)
      pure kept

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
