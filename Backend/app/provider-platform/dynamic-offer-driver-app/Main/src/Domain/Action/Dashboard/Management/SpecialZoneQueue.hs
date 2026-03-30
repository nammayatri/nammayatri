{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.Dashboard.Management.SpecialZoneQueue
  ( postSpecialZoneQueueTriggerNotify,
    getSpecialZoneQueueQueueStats,
  )
where

import qualified API.Types.ProviderPlatform.Management.SpecialZoneQueue as SZQT
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.External.Maps.Types (LatLong (..))
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Queries.GateInfo as QGI
import qualified Lib.Queries.SpecialLocation as QSL
import SharedLogic.Merchant (findMerchantByShortId)
import qualified SharedLogic.External.LocationTrackingService.Flow as LTSFlow
import qualified SharedLogic.SpecialZoneDriverDemand as SpecialZoneDriverDemand
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Tools.Error

postSpecialZoneQueueTriggerNotify :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> SZQT.TriggerSpecialZoneQueueNotifyReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postSpecialZoneQueueTriggerNotify merchantShortId opCity req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  gate <- Esq.runInReplica (QGI.findById (Kernel.Types.Id.Id req.gateId)) >>= fromMaybeM (InvalidRequest $ "Gate not found: " <> req.gateId)
  notifiedCount <- SpecialZoneDriverDemand.forceNotifyDriverDemand merchantOpCity.id merchant.id gate req.vehicleType req.driversToNotify
  logInfo $ "Dashboard trigger: notified " <> show notifiedCount <> " drivers for gate " <> req.gateId
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
  -- Get queue stats per vehicle type
  let vehicleTypes = ["SUV", "SEDAN", "HATCHBACK", "AUTO_RICKSHAW", "BIKE", "AMBULANCE"]
  vehicleStats <- forM vehicleTypes $ \vt -> do
    queueResp <- LTSFlow.getQueueDrivers specialLocationId vt
    let totalInQueue = queueResp.queueSize
        queuedDriverIds = map (.driverId) queueResp.drivers
        -- Drivers that are both in this vehicle type's queue AND inside the pickup zone
        inPickupZone = length $ filter (`elem` pickupZoneDriverIds) queuedDriverIds
        outsidePickupZone = totalInQueue - inPickupZone
    pure SZQT.VehicleQueueStats
      { vehicleType = vt,
        totalInQueue = totalInQueue,
        inPickupZone = inPickupZone,
        outsidePickupZone = max 0 outsidePickupZone
      }
  let nonEmptyStats = filter (\s -> s.totalInQueue > 0) vehicleStats
      totalDrivers = sum $ map (.totalInQueue) nonEmptyStats
      totalInZone = sum $ map (.inPickupZone) nonEmptyStats
      totalOutside = sum $ map (.outsidePickupZone) nonEmptyStats
  pure SZQT.SpecialZoneQueueStatsRes
    { gateId = gateId,
      gateName = gate.name,
      specialLocationName = specialLocationName,
      vehicleStats = nonEmptyStats,
      totalDriversInQueue = totalDrivers,
      totalInPickupZone = totalInZone,
      totalOutsidePickupZone = totalOutside
    }
  where
    isInsideGateGeometry gateInfoId driverLoc = do
      mbGate <- Esq.runInReplica $ QGI.findGateInfoIfDriverInsideGatePickupZone (LatLong driverLoc.lat driverLoc.lon)
      pure $ case mbGate of
        Just g -> g.id == gateInfoId
        Nothing -> False
