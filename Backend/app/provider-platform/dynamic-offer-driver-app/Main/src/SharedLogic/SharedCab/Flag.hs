-- | Shared-cab session flag (driver_information.shared_cab_session_active):
--   the ONE exported writer for the whole driver-app (R26, task 4.x hardening).
--
--   The flag is fail-closed: True excludes the driver from the plain-taxi
--   pool until a session-end path writes False. Every write must therefore
--   walk the exact same route:
--
--     * 'QDriverInformationExtra.updateSharedCabSessionActive' — the
--       authoritative choke point: DB update + LTS shadow sync, never a
--       side-flip of the mirrored value;
--
--     * inside 'Redis.runInMasterCloudRedisCellWithCrossAppRedis'
--       ('withMasterRedis'): the write lands in the cross-app master Redis
--       cell, on master, because the writer can be ANY driver-app service
--       (the UI Flow on Main, the Allocator scheduler) each with its own
--       key prefix — withCrossAppRedis strips the prefix so all services
--       mutate the same LTS state, in the same cell, read-mostly by the
--       pool logic.
--
--   Callers: Domain.Action.UI.SharedCab (driver select sets True BEFORE the
--   BAP call; post-END clears) and SharedLogic.Allocator.Jobs.SharedCab
--   .Reconciler (stale-flag sweeper clears on an unambiguous end signal).
--   Nothing else is allowed to call updateSharedCabSessionActive — grep it.
module SharedLogic.SharedCab.Flag
  ( setSharedCabSessionActive,
    clearSharedCabSessionActive,
  )
where

import qualified Domain.Types.Person as DP
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.DriverInformationExtra as QDriverInformationExtra

setSharedCabSessionActive ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    Redis.HedisLTSFlowEnv r
  ) =>
  Id DP.Person ->
  m ()
setSharedCabSessionActive = writeSharedCabSessionActive True

clearSharedCabSessionActive ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    Redis.HedisLTSFlowEnv r
  ) =>
  Id DP.Person ->
  m ()
clearSharedCabSessionActive = writeSharedCabSessionActive False

writeSharedCabSessionActive ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    Redis.HedisLTSFlowEnv r
  ) =>
  Bool ->
  Id DP.Person ->
  m ()
writeSharedCabSessionActive active driverId =
  Redis.runInMasterCloudRedisCellWithCrossAppRedis . Redis.withMasterRedis $
    QDriverInformationExtra.updateSharedCabSessionActive active driverId
