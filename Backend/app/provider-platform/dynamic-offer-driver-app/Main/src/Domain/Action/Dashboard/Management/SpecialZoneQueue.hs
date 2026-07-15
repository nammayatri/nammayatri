{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.Dashboard.Management.SpecialZoneQueue
  ( postSpecialZoneQueueTriggerNotify,
    getSpecialZoneQueueTriggerNotifyStatus,
    getSpecialZoneQueueQueueStats,
    postSpecialZoneQueueManualQueueAdd,
    postSpecialZoneQueueManualQueueRemove,
    getSpecialZoneQueueDriverQueuePosition,
    getSpecialZoneQueueDriverQueueHistory,
  )
where

import qualified API.Types.ProviderPlatform.Management.SpecialZoneQueue as SZQT
import Data.List (nub)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Time (NominalDiffTime, addUTCTime)
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person as DP
import qualified Domain.Types.ServiceTierType as DVST
import qualified Domain.Types.SpecialZoneQueueRequest as DSZQR
import Domain.Types.VehicleVariant (castServiceTierToVariant)
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.Prelude (read)
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Common (Seconds (..))
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Queries.GateInfo as QGI
import qualified Lib.Queries.SpecialLocation as QSL
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import qualified Lib.Types.GateInfoExtra as DGI
import SharedLogic.Allocator (AllocatorJobType (..), TriggerSpecialZoneNotifyJobData (..))
import qualified SharedLogic.External.LocationTrackingService.Flow as LTSFlow
import SharedLogic.Merchant (findMerchantByShortId)
import qualified SharedLogic.SpecialZoneDriverDemand as SpecialZoneDriverDemand
import Storage.Beam.SchedulerJob ()
import Storage.Beam.SpecialZone ()
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.VehicleServiceTier as CQVST
import qualified Storage.Queries.SpecialZoneQueueRequestExtra as QSZQR
import Tools.Error

-- | Dashboard trigger: previously ran the full force-notify pipeline (LTS queue
-- lookup, eligibility filtering, FCM/GRPC fan-out) synchronously, which made the
-- request slow. Now it only validates the gate, mints a triggerRequestId, marks it
-- active, and schedules a self-rescheduling 'TriggerSpecialZoneNotify' job that does
-- the heavy work in the allocator and retries every request-validity window until
-- the required accepts are reached or 5 minutes elapse. The dashboard watches
-- progress via the parameterless 'getSpecialZoneQueueTriggerNotifyStatus'.
postSpecialZoneQueueTriggerNotify :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> SZQT.TriggerSpecialZoneQueueNotifyReq -> Environment.Flow SZQT.TriggerSpecialZoneQueueNotifyRes)
postSpecialZoneQueueTriggerNotify merchantShortId opCity req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  -- Fetch the gate up-front: validates the id (immediate dashboard feedback) and
  -- gives us the request-validity config to use as the retry cadence.
  gate <- QGI.findById (Kernel.Types.Id.Id req.gateId) >>= fromMaybeM (InvalidRequest $ "Gate not found: " <> req.gateId)
  triggerRequestId <- generateGUID
  now <- getCurrentTime
  let retryIntervalSec = fromMaybe 15 gate.pickupRequestResponseTimeoutInSec
      retryTill = addUTCTime triggerRetryWindowSec now
  -- Stash the target + mark active so the (parameterless) status endpoint can find it.
  SpecialZoneDriverDemand.setTriggerRequired triggerRequestId req.driversToNotify
  SpecialZoneDriverDemand.setTriggerGate triggerRequestId req.gateId
  SpecialZoneDriverDemand.addActiveTrigger merchantOpCity.id triggerRequestId
  createJobIn @_ @'TriggerSpecialZoneNotify
    (Just merchant.id)
    (Just merchantOpCity.id)
    (secondsToNominalDiffTime $ Seconds 0)
    TriggerSpecialZoneNotifyJobData
      { triggerRequestId = triggerRequestId,
        gateId = req.gateId,
        vehicleType = req.vehicleType,
        driversToNotify = req.driversToNotify,
        forceNotifyDriverIds = req.forceNotifyDriverIds,
        isDemandHigh = req.isDemandHigh,
        merchantId = merchant.id,
        merchantOperatingCityId = merchantOpCity.id,
        retryIntervalSec = retryIntervalSec,
        retryTill = retryTill
      }
  logInfo $ "Dashboard trigger scheduled: triggerRequestId=" <> triggerRequestId <> " for gate " <> req.gateId
  pure SZQT.TriggerSpecialZoneQueueNotifyRes {requestId = triggerRequestId}

-- | Status of all currently-active dashboard triggers for this city. Served purely
-- from Redis counters (no table scan) — a handful of GETs + a ZCARD/ZCOUNT per
-- active trigger. Also drains the cleanup set (triggers whose retry loop has just
-- stopped): for each it settles any still-Active rows to Ignored in the DB once,
-- then removes it from cleanup. Counts: accepted = net committed accepts; pending =
-- requests still inside their response window; ignored = requests that expired
-- (validTill passed) without a response.
getSpecialZoneQueueTriggerNotifyStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.Flow SZQT.TriggerSpecialZoneQueueNotifyStatusRes)
getSpecialZoneQueueTriggerNotifyStatus merchantShortId opCity = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  now <- getCurrentTime
  -- Cleanup pass: settle finished triggers' leftover rows to Ignored, then forget them.
  cleanupIds <- SpecialZoneDriverDemand.getCleanupTriggers merchantOpCity.id
  forM_ cleanupIds $ \trid -> do
    QSZQR.markRemainingIgnoredByTriggerRequestId trid
    SpecialZoneDriverDemand.removeCleanupTrigger merchantOpCity.id trid
  -- Live snapshot of every still-active trigger, from Redis counters only.
  activeIds <- SpecialZoneDriverDemand.getActiveTriggers merchantOpCity.id
  activeRequests <- forM activeIds $ \trid -> do
    counts <- SpecialZoneDriverDemand.getTriggerCounts trid now
    pure
      SZQT.TriggerSpecialZoneQueueNotifyStatus
        { requestId = trid,
          gateId = counts.gateId,
          requiredAccepts = counts.requiredAccepts,
          totalRequested = counts.totalRequested,
          accepted = counts.accepted,
          rejected = counts.rejected,
          pendingResponse = counts.pendingResponse,
          ignored = counts.ignored
        }
  pure SZQT.TriggerSpecialZoneQueueNotifyStatusRes {activeRequests = activeRequests}

-- | Retry window for a dashboard trigger: keep topping up notifications for 5 minutes.
triggerRetryWindowSec :: NominalDiffTime
triggerRetryWindowSec = 5 * 60

postSpecialZoneQueueManualQueueAdd :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> SZQT.ManualQueueAddReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postSpecialZoneQueueManualQueueAdd merchantShortId opCity req = do
  merchant <- findMerchantByShortId merchantShortId
  _merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  let driverId = Kernel.Types.Id.Id req.driverId :: Kernel.Types.Id.Id DP.Person
  -- Remove first (if already in queue), then add at desired position
  logError $
    "[postSpecialZoneQueueManualQueueAdd] manualQueueRemove triggered: driverId=" <> req.driverId
      <> ", specialLocationId="
      <> req.specialLocationId
      <> ", vehicleType="
      <> req.vehicleType
      <> ", queuePosition="
      <> show req.queuePosition
      <> ", reason=dashboard manual queue add (cleanup before re-add)"
  void $ LTSFlow.manualQueueRemove req.specialLocationId req.vehicleType merchant.id driverId (Just "dashboard_re_add")
  void $ LTSFlow.manualQueueAdd req.specialLocationId req.vehicleType merchant.id driverId req.queuePosition
  logInfo $ "Dashboard: added driver " <> req.driverId <> " to queue at position " <> show req.queuePosition <> " for " <> req.specialLocationId <> "/" <> req.vehicleType
  pure Kernel.Types.APISuccess.Success

postSpecialZoneQueueManualQueueRemove :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> SZQT.ManualQueueRemoveReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postSpecialZoneQueueManualQueueRemove merchantShortId opCity req = do
  merchant <- findMerchantByShortId merchantShortId
  _merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  let driverId = Kernel.Types.Id.Id req.driverId :: Kernel.Types.Id.Id DP.Person
  logError $
    "[postSpecialZoneQueueManualQueueRemove] manualQueueRemove triggered: driverId=" <> req.driverId
      <> ", specialLocationId="
      <> req.specialLocationId
      <> ", vehicleType="
      <> req.vehicleType
      <> ", reason=dashboard manual queue remove"
  void $ LTSFlow.manualQueueRemove req.specialLocationId req.vehicleType merchant.id driverId (Just "dashboard_manual_remove")
  logInfo $ "Dashboard: removed driver " <> req.driverId <> " from queue for " <> req.specialLocationId <> "/" <> req.vehicleType
  pure Kernel.Types.APISuccess.Success

getSpecialZoneQueueQueueStats :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Text -> Environment.Flow SZQT.SpecialZoneQueueStatsRes)
getSpecialZoneQueueQueueStats merchantShortId opCity gateId = do
  merchant <- findMerchantByShortId merchantShortId
  _merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  gate <- QGI.findById (Kernel.Types.Id.Id gateId) >>= fromMaybeM (InvalidRequest $ "Gate not found: " <> gateId)
  let specialLocationId = gate.specialLocationId.getId
  mbSpecialLocation <- QSL.findById (Kernel.Types.Id.Id specialLocationId)
  let specialLocationName = maybe "" (.locationName) mbSpecialLocation
  -- Get all drivers near gate once, filter to those inside pickup zone
  driversNearGate <- LTSFlow.nearBy gate.point.lat gate.point.lon (Just False) Nothing 2500 merchant.id Nothing Nothing
  driversInPickupZone <- filterM (isInsideGateGeometry gate.id) driversNearGate
  let pickupZoneDriverIds = map (.driverId) driversInPickupZone
  -- Compute the queue-request lookback cutoff once, reused across all variants.
  now <- getCurrentTime
  let queueRequestCutoff = addUTCTime (negate (2 * 60 * 60)) now
  -- Fetch VST configs for callout variant mapping.
  cityServiceTiers <- CQVST.findAllByMerchantOpCityId _merchantOpCity.id (Just specialLocationId)
  let -- Build callout variants per VST, fallback to castServiceTierToVariant
      getCalloutVars vst =
        if null vst.specialZoneQueueCalloutVariants
          then [castServiceTierToVariant vst.serviceTierType]
          else vst.specialZoneQueueCalloutVariants
      uniqVariantsList = nub $ concatMap getCalloutVars cityServiceTiers
  uniqVariantsQueueList <- mapM (\vt' -> do resp <- LTSFlow.getQueueDrivers specialLocationId (show vt'); pure (vt', resp)) uniqVariantsList
  let uniqVariantsQueueMap = Map.fromList uniqVariantsQueueList

  let driverLocMap = Map.fromList $ map (\dl -> (dl.driverId.getId, dl)) driversNearGate
  vehicleStats <- forM cityServiceTiers $ \vst -> do
    let vt = show vst.serviceTierType
        calloutVars = getCalloutVars vst
        queueResps = mapMaybe (\cv -> Map.lookup cv uniqVariantsQueueMap) calloutVars
        -- Merge queue responses from all callout variants for this tier
        mergedDrivers = concatMap (.drivers) queueResps
        mergedQueueSize = sum $ map (.queueSize) queueResps
    -- Live demand (pending customer searches) and committed supply (drivers notified/accepted)
    mbDemandCount <- Redis.runInMasterCloudRedisCellWithCrossAppRedis $ Redis.get @Int (SpecialZoneDriverDemand.mkGateSearchDemandKey gateId vt)
    mbSupplyCount <- Redis.runInMasterCloudRedisCellWithCrossAppRedis $ Redis.get @Int (SpecialZoneDriverDemand.mkGateSearchSupplyKey gateId vt)
    -- Drivers who accepted the pickup notification and are en-route to the gate
    acceptedRequests <- QSZQR.findAllByGateIdStatusAndVehicleType queueRequestCutoff gateId DSZQR.Accepted vt
    let totalInQueue = mergedQueueSize
        queuedDriverIds = map (.driverId) mergedDrivers
        -- Drivers that are both in this vehicle type's queue AND inside the pickup zone
        inPickupZone = length $ filter (`elem` pickupZoneDriverIds) queuedDriverIds
        outsidePickupZone = totalInQueue - inPickupZone
        acceptedDriverIds = map (\r -> r.driverId.getId) acceptedRequests
        -- Check per-driver notified key to determine committedToPickup
        notifiedDriverIds = acceptedDriverIds -- drivers who accepted are committed
        -- Build per-driver details with location from the nearBy data
        driverDetails = mapMaybe (mkDriverDetail driverLocMap pickupZoneDriverIds acceptedDriverIds notifiedDriverIds) mergedDrivers
    pure
      SZQT.VehicleQueueStats
        { vehicleType = vt,
          specialZoneTierLabel = vst.name,
          totalInQueue = totalInQueue,
          inPickupZone = inPickupZone,
          outsidePickupZone = max 0 outsidePickupZone,
          pendingDemand = fromMaybe 0 mbDemandCount,
          driversCommittedToPickup = fromMaybe 0 mbSupplyCount,
          acceptedQueueRequests = length acceptedRequests,
          demandThreshold = DGI.demandThresholdFor gate vt,
          minDriverThreshold = DGI.minDriverThresholdFor gate vt,
          maxDriverThreshold = DGI.maxDriverThresholdFor gate vt,
          drivers = driverDetails
        }
  let nonEmptyStats = filter (isNonEmpty gate) vehicleStats
      totalDrivers = sum $ map (.totalInQueue) nonEmptyStats
      totalInZone = sum $ map (.inPickupZone) nonEmptyStats
      totalOutside = sum $ map (.outsidePickupZone) nonEmptyStats
      totalDemand = sum $ map (.pendingDemand) nonEmptyStats
      totalCommitted = sum $ map (.driversCommittedToPickup) nonEmptyStats
      totalAccepted = sum $ map (.acceptedQueueRequests) nonEmptyStats
  pure
    SZQT.SpecialZoneQueueStatsRes
      { gateId = gateId,
        gateName = gate.name,
        specialLocationName = specialLocationName,
        canQueueUpOnGate = gate.canQueueUpOnGate,
        vehicleStats = nonEmptyStats,
        totalDriversInQueue = totalDrivers,
        totalInPickupZone = totalInZone,
        totalOutsidePickupZone = totalOutside,
        totalPendingDemand = totalDemand,
        totalDriversCommittedToPickup = totalCommitted,
        totalAcceptedQueueRequests = totalAccepted
      }
  where
    isInsideGateGeometry gateInfoId driverLoc = do
      mbGate <- QGI.findGateInfoIfDriverInsideGatePickupZone (LatLong driverLoc.lat driverLoc.lon)
      pure $ case mbGate of
        Just g -> g.id == gateInfoId
        Nothing -> False
    -- Keep a variant if it has any live activity OR any explicit per-variant config
    -- (so dashboards can always see what the operator has intentionally configured).
    hasVariantConfig gate variant =
      isJust (Map.lookup variant =<< gate.minDriverThresholds)
        || isJust (Map.lookup variant =<< gate.maxDriverThresholds)
        || isJust (Map.lookup variant =<< gate.demandThresholds)
    isNonEmpty gate s =
      s.totalInQueue > 0
        || s.pendingDemand > 0
        || s.driversCommittedToPickup > 0
        || s.acceptedQueueRequests > 0
        || hasVariantConfig gate s.vehicleType
    mkDriverDetail driverLocMap pzDriverIds acceptedIds committedIds queueEntry =
      let did = queueEntry.driverId.getId
       in case Map.lookup did driverLocMap of
            Just dl ->
              Just
                SZQT.QueueDriverDetail
                  { driverId = did,
                    lat = dl.lat,
                    lon = dl.lon,
                    queuePosition = Just queueEntry.queuePosition,
                    inPickupZone = queueEntry.driverId `elem` pzDriverIds,
                    acceptedQueueRequest = did `elem` acceptedIds,
                    committedToPickup = did `elem` committedIds
                  }
            Nothing -> Nothing

getSpecialZoneQueueDriverQueuePosition :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Text -> Text -> Text -> Environment.Flow SZQT.DriverQueuePositionRes)
getSpecialZoneQueueDriverQueuePosition merchantShortId opCity driverIdText specialLocationId vehicleType = do
  _merchant <- findMerchantByShortId merchantShortId
  _merchantOpCity <- CQMOC.findByMerchantIdAndCity _merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  let driverId = Kernel.Types.Id.Id driverIdText :: Kernel.Types.Id.Id DP.Person
  posResp <- LTSFlow.getQueueDriverPosition specialLocationId vehicleType driverId
  let mbPos = case posResp.queuePositionRange of
        Just (lo, _hi) -> Just lo
        Nothing -> Nothing
  pure
    SZQT.DriverQueuePositionRes
      { queuePosition = mbPos,
        queueSize = posResp.queueSize
      }

getSpecialZoneQueueDriverQueueHistory :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Text -> Environment.Flow SZQT.DriverQueueHistoryRes)
getSpecialZoneQueueDriverQueueHistory merchantShortId opCity driverIdText = do
  merchant <- findMerchantByShortId merchantShortId
  _merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  let driverId = Kernel.Types.Id.Id driverIdText :: Kernel.Types.Id.Id DP.Person
  ltsResp <- LTSFlow.getDriverQueueHistory merchant.id driverId
  pure
    SZQT.DriverQueueHistoryRes
      { trackingState = fmap mkTrackingState ltsResp.trackingState,
        currentRank = ltsResp.currentRank,
        events = map mkEvent ltsResp.events
      }
  where
    mkTrackingState ts =
      SZQT.QueueTrackingState
        { specialLocationId = ts.specialLocationId,
          vehicleType = ts.vehicleType,
          consecutiveExitPings = ts.consecutiveExitPings,
          lastRecordedRank = ts.lastRecordedRank
        }
    mkEvent ev =
      SZQT.QueueHistoryEvent
        { timestamp = ev.timestamp,
          value = ev.value
        }
