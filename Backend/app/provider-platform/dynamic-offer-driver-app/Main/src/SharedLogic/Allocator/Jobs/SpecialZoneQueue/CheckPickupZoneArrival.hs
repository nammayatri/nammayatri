module SharedLogic.Allocator.Jobs.SpecialZoneQueue.CheckPickupZoneArrival
  ( checkPickupZoneArrival,
  )
where

import qualified Domain.Types.SpecialZoneQueueRequest as DSZQR
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Queries.GateInfo as QGI
import Lib.Scheduler
import SharedLogic.Allocator (AllocatorJobType (..))
import qualified SharedLogic.External.LocationTrackingService.Flow as LTSFlow
import SharedLogic.External.LocationTrackingService.Types (HasLocationService)
import qualified Storage.Queries.SpecialZoneQueueRequest as QSZQR

checkPickupZoneArrival ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    Esq.EsqDBReplicaFlow m r,
    MonadFlow m,
    HasLocationService m r,
    HasShortDurationRetryCfg r c,
    HasRequestId r
  ) =>
  Job 'CheckPickupZoneArrival ->
  m ExecutionResult
checkPickupZoneArrival Job {id = jobId, jobInfo} = withLogTag ("JobId-" <> jobId.getId) do
  let jobData = jobInfo.jobData
      driverId = jobData.driverId
      gateId = jobData.gateId
      specialLocationId = jobData.specialLocationId
      vehicleType = jobData.vehicleType
      merchantId = jobData.merchantId
  -- Check if request still exists and hasn't already been marked NoShow/Ignored
  mbRequest <- QSZQR.findByPrimaryKey (Id jobData.requestId)
  case mbRequest of
    Nothing -> do
      logInfo $ "Request " <> jobData.requestId <> " not found, skipping"
      return Complete
    Just request
      | request.response == Just DSZQR.NoShow -> do
          logInfo $ "Request " <> jobData.requestId <> " already marked NoShow, skipping"
          return Complete
      | otherwise -> do
          mbGate <- Esq.runInReplica $ QGI.findById (Id gateId)
          case mbGate of
            Nothing -> do
              logWarning $ "Gate " <> gateId <> " not found for arrival check"
              return Complete
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
                  return Complete
                else do
                  logWarning $ "Driver " <> driverId.getId <> " no-show at gate " <> gateId <> ", removing from queue"
                  -- Remove from queue first; if this fails, job will retry and NoShow won't be set
                  void $ LTSFlow.manualQueueRemove specialLocationId vehicleType merchantId driverId
                  QSZQR.updateResponse (Just DSZQR.NoShow) DSZQR.Expired (Id jobData.requestId)
                  return Complete
