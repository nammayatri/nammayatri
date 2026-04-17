{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.Dashboard.Management.SpecialZoneQueue
  ( postSpecialZoneQueueTriggerNotify,
    getSpecialZoneQueueQueueStats,
    postSpecialZoneQueueManualQueueAdd,
    postSpecialZoneQueueManualQueueRemove,
  )
where

import qualified API.Types.ProviderPlatform.Management.SpecialZoneQueue as SZQT
import Data.List (nub)
import qualified Data.Map.Strict as Map
import Data.Time (addUTCTime)
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person as DP
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.External.Maps.Types (LatLong (..))
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Queries.GateInfo as QGI
import qualified Lib.Queries.SpecialLocation as QSL
import qualified Lib.Types.GateInfo as DGI
import qualified SharedLogic.External.LocationTrackingService.Flow as LTSFlow
import SharedLogic.Merchant (findMerchantByShortId)
import qualified SharedLogic.SpecialZoneDriverDemand as SpecialZoneDriverDemand
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.SpecialZoneQueueRequestExtra as QSZQR
import Tools.Error

postSpecialZoneQueueTriggerNotify :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> SZQT.TriggerSpecialZoneQueueNotifyReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postSpecialZoneQueueTriggerNotify merchantShortId opCity req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  gate <- Esq.runInReplica (QGI.findById (Kernel.Types.Id.Id req.gateId)) >>= fromMaybeM (InvalidRequest $ "Gate not found: " <> req.gateId)
  let mbPriorityIds = fmap (map Kernel.Types.Id.Id) req.forceNotifyDriverIds
  totalNotified <- SpecialZoneDriverDemand.forceNotifyDriverDemand merchantOpCity.id merchant.id gate req.vehicleType req.driversToNotify mbPriorityIds
  logInfo $ "Dashboard trigger: notified " <> show totalNotified <> " drivers for gate " <> req.gateId
  pure Kernel.Types.APISuccess.Success

postSpecialZoneQueueManualQueueAdd :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> SZQT.ManualQueueAddReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postSpecialZoneQueueManualQueueAdd merchantShortId opCity req = do
  merchant <- findMerchantByShortId merchantShortId
  _merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  let driverId = Kernel.Types.Id.Id req.driverId :: Kernel.Types.Id.Id DP.Person
  -- Remove first (if already in queue), then add at desired position
  void $ LTSFlow.manualQueueRemove req.specialLocationId req.vehicleType merchant.id driverId
  void $ LTSFlow.manualQueueAdd req.specialLocationId req.vehicleType merchant.id driverId req.queuePosition
  logInfo $ "Dashboard: added driver " <> req.driverId <> " to queue at position " <> show req.queuePosition <> " for " <> req.specialLocationId <> "/" <> req.vehicleType
  pure Kernel.Types.APISuccess.Success

postSpecialZoneQueueManualQueueRemove :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> SZQT.ManualQueueRemoveReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postSpecialZoneQueueManualQueueRemove merchantShortId opCity req = do
  merchant <- findMerchantByShortId merchantShortId
  _merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  let driverId = Kernel.Types.Id.Id req.driverId :: Kernel.Types.Id.Id DP.Person
  void $ LTSFlow.manualQueueRemove req.specialLocationId req.vehicleType merchant.id driverId
  logInfo $ "Dashboard: removed driver " <> req.driverId <> " from queue for " <> req.specialLocationId <> "/" <> req.vehicleType
  pure Kernel.Types.APISuccess.Success

getSpecialZoneQueueQueueStats :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Text -> Environment.Flow SZQT.SpecialZoneQueueStatsRes)
getSpecialZoneQueueQueueStats merchantShortId opCity gateId = do
  merchant <- findMerchantByShortId merchantShortId
  _merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  gate <- Esq.runInReplica (QGI.findById (Kernel.Types.Id.Id gateId)) >>= fromMaybeM (InvalidRequest $ "Gate not found: " <> gateId)
  let specialLocationId = gate.specialLocationId.getId
  mbSpecialLocation <- Esq.runInReplica $ QSL.findById (Kernel.Types.Id.Id specialLocationId)
  let specialLocationName = maybe "" (.locationName) mbSpecialLocation
  -- Get all drivers near gate once, filter to those inside pickup zone
  driversNearGate <- LTSFlow.nearBy gate.point.lat gate.point.lon (Just False) Nothing 500 merchant.id Nothing Nothing
  driversInPickupZone <- filterM (isInsideGateGeometry gate.id) driversNearGate
  let pickupZoneDriverIds = map (.driverId) driversInPickupZone
  -- Compute the queue-request lookback cutoff once, reused across all variants.
  now <- getCurrentTime
  let queueRequestCutoff = addUTCTime (negate (2 * 60 * 60)) now
  -- Base variants + every variant the operator has explicitly configured on the gate
  -- (via per-variant threshold maps). Without this the hardcoded list silently drops
  -- custom variants like SUV_PLUS / TAXI_PLUS so their config never reaches the dashboard.
  let baseVariants = ["SUV", "SEDAN", "HATCHBACK", "AUTO_RICKSHAW", "BIKE", "AMBULANCE"]
      configuredVariants =
        concatMap Map.keys $
          catMaybes [gate.minDriverThresholds, gate.maxDriverThresholds, gate.demandThresholds]
      vehicleTypes = nub (baseVariants <> configuredVariants)
  vehicleStats <- forM vehicleTypes $ \vt -> do
    queueResp <- LTSFlow.getQueueDrivers specialLocationId vt
    let totalInQueue = queueResp.queueSize
        queuedDriverIds = map (.driverId) queueResp.drivers
        -- Drivers that are both in this vehicle type's queue AND inside the pickup zone
        inPickupZone = length $ filter (`elem` pickupZoneDriverIds) queuedDriverIds
        outsidePickupZone = totalInQueue - inPickupZone
    -- Live demand (pending customer searches) and committed supply (drivers notified/accepted)
    mbDemandCount <- Redis.withCrossAppRedis $ Redis.get @Int (SpecialZoneDriverDemand.mkGateSearchDemandKey gateId vt)
    mbSupplyCount <- Redis.withCrossAppRedis $ Redis.get @Int (SpecialZoneDriverDemand.mkGateSearchSupplyKey gateId vt)
    -- Drivers who accepted the pickup notification and are en-route to the gate
    acceptedRequests <- QSZQR.findAllByGateIdStatusAndVehicleType queueRequestCutoff gateId DSZQR.Accepted vt
    pure
      SZQT.VehicleQueueStats
        { vehicleType = vt,
          totalInQueue = totalInQueue,
          inPickupZone = inPickupZone,
          outsidePickupZone = max 0 outsidePickupZone,
          pendingDemand = fromMaybe 0 mbDemandCount,
          driversCommittedToPickup = fromMaybe 0 mbSupplyCount,
          acceptedQueueRequests = length acceptedRequests,
          demandThreshold = DGI.demandThresholdFor gate vt,
          minDriverThreshold = DGI.minDriverThresholdFor gate vt,
          maxDriverThreshold = DGI.maxDriverThresholdFor gate vt
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
      mbGate <- Esq.runInReplica $ QGI.findGateInfoIfDriverInsideGatePickupZone (LatLong driverLoc.lat driverLoc.lon)
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
