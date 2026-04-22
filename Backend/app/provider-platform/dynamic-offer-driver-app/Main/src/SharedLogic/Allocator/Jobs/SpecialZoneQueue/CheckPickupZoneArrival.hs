module SharedLogic.Allocator.Jobs.SpecialZoneQueue.CheckPickupZoneArrival
  ( checkPickupZoneArrival,
    runArrivalCheckForRequest,
  )
where

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
import Lib.Scheduler
import SharedLogic.Allocator (AllocatorJobType (..))
import qualified SharedLogic.External.LocationTrackingService.Flow as LTSFlow
import SharedLogic.External.LocationTrackingService.Types (HasLocationService)
import SharedLogic.SpecialZoneDriverDemand (mkSpecialZoneQueueRequestLockKey)
import qualified SharedLogic.SpecialZoneDriverDemand as SpecialZoneDriverDemand
import qualified Storage.Queries.SpecialZoneQueueRequest as QSZQR
import qualified Tools.Notifications as Notify

checkPickupZoneArrival ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    Esq.EsqDBReplicaFlow m r,
    MonadFlow m,
    MonadMask m,
    HasLocationService m r,
    HasShortDurationRetryCfg r c,
    HasRequestId r
  ) =>
  Job 'CheckPickupZoneArrival ->
  m ExecutionResult
checkPickupZoneArrival Job {id = jobId, jobInfo} = withLogTag ("JobId-" <> jobId.getId) $ do
  let jobData = jobInfo.jobData
  runArrivalCheckForRequest
    (Id jobData.requestId)
    jobData.driverId
    jobData.gateId
    jobData.specialLocationId
    jobData.vehicleType
    jobData.merchantId
    jobData.merchantOperatingCityId
  return Complete

runArrivalCheckForRequest ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    Esq.EsqDBReplicaFlow m r,
    MonadFlow m,
    MonadMask m,
    HasLocationService m r,
    HasShortDurationRetryCfg r c,
    HasRequestId r
  ) =>
  Id DSZQR.SpecialZoneQueueRequest ->
  Id DP.Person ->
  Text -> -- gateId
  Text -> -- specialLocationId
  Text -> -- vehicleType
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  m ()
runArrivalCheckForRequest requestId driverId gateId specialLocationId vehicleType merchantId merchantOpCityId = do
  Redis.whenWithLockRedis (mkSpecialZoneQueueRequestLockKey requestId.getId) 60 $ do
    mbRequest <- QSZQR.findByPrimaryKey requestId
    case mbRequest of
      Nothing -> logInfo $ "Request " <> requestId.getId <> " not found, skipping"
      Just request
        | request.status == DSZQR.Expired ->
          logInfo $ "Request " <> requestId.getId <> " already expired, skipping"
        | request.status == DSZQR.Completed ->
          logInfo $ "Request " <> requestId.getId <> " already completed (ride started), skipping"
        | request.response == Just DSZQR.NoShow ->
          logInfo $ "Request " <> requestId.getId <> " already marked NoShow, skipping"
        | otherwise -> do
          mbGate <- Esq.runInReplica $ QGI.findById (Id gateId)
          case mbGate of
            Nothing -> do
              logWarning $ "Gate " <> gateId <> " not found for arrival check, expiring request"
              QSZQR.updateResponse (Just DSZQR.Ignored) DSZQR.Expired requestId
            Just gate -> do
              driversNearGate <- LTSFlow.nearBy gate.point.lat gate.point.lon (Just False) Nothing 500 merchantId Nothing Nothing
              isInPickupZone <- case find (\d -> d.driverId == driverId) driversNearGate of
                Just dl -> do
                  mbGateCheck <- Esq.runInReplica $ QGI.findGateInfoIfDriverInsideGatePickupZone (LatLong dl.lat dl.lon)
                  pure $ case mbGateCheck of
                    Just g -> g.id == gate.id
                    Nothing -> False
                Nothing -> pure False
              if isInPickupZone
                then do
                  logInfo $ "Driver " <> driverId.getId <> " arrived at pickup zone gate " <> gateId
                  QSZQR.updateResponse (Just DSZQR.Accept) DSZQR.Expired requestId
                else do
                  logWarning $ "Driver " <> driverId.getId <> " no-show at gate " <> gateId <> ", removing from queue"
                  void $ LTSFlow.manualQueueRemove specialLocationId vehicleType merchantId driverId
                  QSZQR.updateResponse (Just DSZQR.NoShow) DSZQR.Expired requestId
                  SpecialZoneDriverDemand.runSupplyDecrementForRequest requestId.getId gateId vehicleType
                  let entityData =
                        Notify.PickupZoneRequestEntityData
                          { requestId = requestId.getId,
                            gateName = gate.name,
                            gateAddress = gate.address,
                            specialLocationName = request.specialLocationName,
                            specialLocationId = specialLocationId,
                            gateId = gateId,
                            vehicleType = vehicleType,
                            validTill = request.validTill,
                            requestType = "PICKUP_ZONE_NO_SHOW"
                          }
                  Notify.notifyPickupNoShow merchantOpCityId driverId entityData
