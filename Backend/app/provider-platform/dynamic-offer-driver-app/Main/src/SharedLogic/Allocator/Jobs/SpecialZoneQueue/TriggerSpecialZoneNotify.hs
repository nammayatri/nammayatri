{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Allocator.Jobs.SpecialZoneQueue.TriggerSpecialZoneNotify
  ( triggerSpecialZoneNotify,
  )
where

import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Queries.GateInfo as QGI
import Lib.Scheduler
import SharedLogic.Allocator (AllocatorJobType (..))
import SharedLogic.External.LocationTrackingService.Types (HasLocationService)
import qualified SharedLogic.SpecialZoneDriverDemand as SpecialZoneDriverDemand
import Storage.Beam.SpecialZone ()

-- | Allocator-side handler for a dashboard /triggerNotify request. Self-reschedules
-- as a retry loop: each cycle notifies enough new drivers to cover the shortfall
-- toward the required accepts, then reschedules itself one 'retryIntervalSec' later
-- (the gate's request-validity window, so the previous batch has expired before we
-- top up). The loop stops — returning 'Complete' and clearing the trigger from the
-- city's active set — as soon as either:
--   * the required number of accepts is reached (read O(1) from a Redis counter,
--     not a table scan), or
--   * the 5-minute retry window ('retryTill') has elapsed.
-- forceNotifyDriverDemand's eligibility filter (cooldown + already-Active/Accepted)
-- guarantees each cycle reaches drivers not already notified, so retries fan out
-- to fresh drivers rather than re-spamming the same ones.
triggerSpecialZoneNotify ::
  ( ServiceFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    Esq.EsqDBReplicaFlow m r,
    MonadFlow m,
    MonadMask m,
    HasLocationService m r,
    HasShortDurationRetryCfg r c,
    HasRequestId r,
    HasFlowEnv m r '["maxNotificationShards" ::: Int],
    Redis.HedisFlow m r,
    Redis.HedisLTSFlowEnv r,
    HasField "gateNotifiedKeyShards" r Int
  ) =>
  Job 'TriggerSpecialZoneNotify ->
  m ExecutionResult
triggerSpecialZoneNotify Job {id = jobId, jobInfo} = withLogTag ("JobId-" <> jobId.getId) $ do
  let d = jobInfo.jobData
      mocId = d.merchantOperatingCityId
      stop reason = do
        logInfo $ "TriggerSpecialZoneNotify stopping (" <> reason <> "): triggerRequestId=" <> d.triggerRequestId
        -- Move active -> cleanup; the status endpoint settles the leftover rows to Ignored.
        SpecialZoneDriverDemand.finalizeTrigger mocId d.triggerRequestId
        pure Complete
  acceptCount <- SpecialZoneDriverDemand.getTriggerAcceptCount d.triggerRequestId
  now <- getCurrentTime
  if acceptCount >= d.driversToNotify
    then stop $ "target reached, accepts=" <> show acceptCount <> "/" <> show d.driversToNotify
    else
      if now >= d.retryTill
        then stop $ "retry window elapsed, accepts=" <> show acceptCount <> "/" <> show d.driversToNotify
        else do
          mbGate <- QGI.findById (Id d.gateId)
          case mbGate of
            Nothing -> stop $ "gate not found id=" <> d.gateId
            Just gate -> do
              let remaining = max 0 (d.driversToNotify - acceptCount)
                  mbPriorityIds = fmap (map Id) d.forceNotifyDriverIds
              totalNotified <-
                SpecialZoneDriverDemand.forceNotifyDriverDemand
                  mocId
                  d.merchantId
                  gate
                  d.vehicleType
                  remaining
                  mbPriorityIds
                  d.isDemandHigh
                  (Just d.triggerRequestId)
              logInfo $
                "TriggerSpecialZoneNotify cycle: triggerRequestId=" <> d.triggerRequestId
                  <> " accepts="
                  <> show acceptCount
                  <> "/"
                  <> show d.driversToNotify
                  <> " notifiedThisCycle="
                  <> show totalNotified
              -- Reschedule the same job (same jobData, unchanged retryTill) for the next cycle.
              pure $ ReSchedule (addUTCTime (fromIntegral d.retryIntervalSec) now)
