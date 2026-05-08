module SharedLogic.Allocator.Jobs.SpecialZoneQueue.RetryTriggerNotify
  ( retryTriggerNotify,
  )
where

import Data.List (sortOn)
import Data.Ord (Down (..))
import qualified Domain.Types.SpecialZoneQueueRequest as DSZQR
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Queries.GateInfo as QGI
import Lib.Scheduler
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import SharedLogic.Allocator (AllocatorJobType (..))
import SharedLogic.External.LocationTrackingService.Types (HasLocationService)
import qualified SharedLogic.SpecialZoneDriverDemand as SZD
import qualified Storage.Queries.SpecialZoneQueueRequestExtra as QSZQR

retryTriggerNotify ::
  ( ServiceFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    Esq.EsqDBReplicaFlow m r,
    BeamFlow m r,
    MonadFlow m,
    MonadMask m,
    HasLocationService m r,
    HasShortDurationRetryCfg r c,
    HasRequestId r,
    HasFlowEnv m r '["maxNotificationShards" ::: Int],
    Redis.HedisFlow m r,
    Redis.HedisLTSFlowEnv r,
    HasField "gateNotifiedKeyShards" r Int,
    JobCreator r m
  ) =>
  Job 'RetryTriggerNotify ->
  m ExecutionResult
retryTriggerNotify Job {id = jobId, jobInfo} = withLogTag ("JobId-" <> jobId.getId) $ do
  let d = jobInfo.jobData
  sessions <- SZD.getActiveTriggerSessions d.gateId d.vehicleType d.maxRetryDurationSec
  if null sessions
    then do
      SZD.releaseRetryJobClaim d.gateId d.vehicleType
      pure Complete
    else do
      mbGate <- QGI.findById (Id d.gateId)
      case mbGate of
        Nothing -> do
          logWarning $ "RetryTriggerNotify: gate not found id=" <> d.gateId
          SZD.releaseRetryJobClaim d.gateId d.vehicleType
          pure Complete
        Just gate -> do
          rows <- QSZQR.findAllByRequestIds (map (.requestId) sessions)
          forM_ (filter ((== DSZQR.Dashboard) . (.source)) sessions) $ \s -> do
            let target = fromMaybe 0 s.target
                acceptedForReq = length $ filter (\r -> r.requestId == Just s.requestId && r.status == DSZQR.Accepted) rows
                gap = target - acceptedForReq
            when (gap > 0) $ do
              notified <-
                SZD.forceNotifyDriverDemand
                  d.merchantOperatingCityId
                  d.merchantId
                  gate
                  d.vehicleType
                  gap
                  (Just s.priorityDriverIds)
                  s.isDemandHigh
                  (Just s.requestId)
              when (notified > 0) $
                SZD.recordTriggerNotifyEvent
                  d.gateId
                  d.vehicleType
                  s.requestId
                  DSZQR.Dashboard
                  notified
                  s.target
                  s.isDemandHigh
                  s.priorityDriverIds
                  d.maxRetryDurationSec
                  SZD.UpdateOnly
          let appSessions = filter ((== DSZQR.App) . (.source)) sessions
          case sortOn (Down . (.startedAt)) appSessions of
            (latest : _) ->
              SZD.checkAndNotifyDriverDemand
                d.merchantOperatingCityId
                d.merchantId
                gate
                d.vehicleType
                Nothing
                (Just DSZQR.App)
                (Just latest.requestId)
                SZD.UpdateOnly
            [] -> pure ()
          createJobIn @_ @'RetryTriggerNotify
            (Just d.merchantId)
            (Just d.merchantOperatingCityId)
            (secondsToNominalDiffTime $ Seconds d.retryIntervalSec)
            d
          pure Complete
