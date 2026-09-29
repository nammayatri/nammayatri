-- | Shared-cab "stuck flag" reconciler job (NY shared-cab-prime, task 4.2C —
--   the STRICTLY fail-closed revision of 4.2B).
--
--   One job per (merchant, city), self-re-enqueued at a fixed interval.
--   driver_information.shared_cab_session_active is fail-closed (a True row
--   excludes the driver from the plain-taxi pool), so a driver whose
--   rider-app session died without the end-session update reaching us would
--   be locked out forever. The reconciler walks every driver in the city
--   whose flag is True, asks the rider-app BAP for the live shared-cab
--   session (GET /internal/sharedCab/session?driverId&vehicleNumber), and
--   flips the flag to False ONLY on an UNAMBIGUOUS end signal.
--
--   THE ONLY clear signal is a DECODED BAP error body (the kernel APIError
--   JSON contract: {errorCode, errorMessage, errorPayload}) whose errorCode
--   is SHARED_CAB_SESSION_NOT_FOUND or SHARED_CAB_SESSION_HELD_BY_ANOTHER_DRIVER
--   — the SharedCabSessionError 'NOT_FOUND'-family the rider-app session
--   handler itself throws (rider-app Tools/Error.hs). Anything else leaves
--   the flag EXACTLY as it was:
--     * a bare HTTP 404/409 (missing route, wrong URL, an nginx/proxy 404):
--       the body does not decode to a kernel APIError, so the call surfaces
--       as ExternalAPICallError (RawError), not the typed APICallError;
--     * 2xx, even 200-with-empty-body: the request reached a live session
--       (ownedSession already gates driverId, so a second person reusing
--       this driverId mid-interleave yields 200, never a clear signal);
--     * a decoded but different error code (auth, version mismatch, ...);
--     * connection errors, timeouts, 5xx, DoS-ish garbage.
--
--   Sweep: keyset pagination (driverId > lastSeen, Asc). The pass shrinks
--   the set it walks, so offset pagination (4.2B) would skip rows; keyset
--   over the ORDER BY column does not. The flag DELETE goes through the
--   authoritative choke point ('QDIExtra.updateSharedCabSessionActive': DB +
--   LTS, never a side-flip of the mirrored value) inside the cross-app
--   master cell: Redis.runInMasterCloudRedisCellWithCrossAppRedis .
--   Redis.withMasterRedis.
--
--   Chain lifecycle: the job ALWAYS re-enqueues itself after a fire — even
--   when the sweep aborts on an escaped exception, so a partial failure can
--   not lose the scheduler chain (cloned into the finally-style sequence
--   below). The only way to stop the chain is TransporterConfig
--   .sharedCabReconcilerEnabled = Nothing/false (fail-closed, no writes, no
--   BAP calls). The first job is created via the dashboard scheduler trigger
--   (Common.SharedCabReconcilerTrigger) — see
--   Domain.Action.Dashboard.Management.Merchant.postMerchantSchedulerTrigger.
--
--   DUPLICATE-CHAIN GUARD (R26): the trigger seed above goes through
--   'seedSharedCabReconcilerChain', so a second seed (dashboard re-trigger)
--   or a redeploy can not start a second self-re-enqueuing chain for the
--   same city:
--
--     1. DB-EXISTENCE: a scheduler_job row of jobType SharedCabReconciler
--        for this city with status Pending means a chain is alive and the
--        seed is skipped. ONLY Pending counts — Completed/Failed rows are
--        dead chain roots and must never block a reseed. NOTE (claude's
--        STEP-0 check, verified in R26): driver-app runs the RedisBased
--        scheduler (dhall schedulerType) and SharedCabReconciler is NOT in
--        jobInfoMap, so under the deployed config job records live only in
--        Redis (zset + stream), the Redis-side lookup fns are stubs that
--        return [], and the table check is VACUOUS — the no-TTL SETNX below
--        is the de-facto primary guard; the DB check becomes the durable
--        truth the day the job is marked long-running or the scheduler
--        flips to DbBased.
--     2. SETNX sharedcab:reconciler:seeded:<city> (no TTL, raw cross-app
--        key in the master cell): wins the first-seed race — Main and the
--        Allocator scheduler use different key prefixes, so the key goes
--        through withCrossAppRedis to strip them. Loser skips; winner
--        creates the first job. If createJob fails after winning, the
--        marker is released again so a later trigger can retry, and the
--        seed call fails loudly instead of returning a false Success.
--
--   The chain's own re-enqueue (below, after every fire) is deliberately
--   NOT guarded: a fire is by definition inside the live chain. When the
--   chain INTENTIONALLY ends (reconciler disabled for the city) the
--   marker is released so ops can re-enable and reseed later; a full
--   Redis flush loses chain + marker together, so the next trigger then
--   reseeds exactly one clean chain.
module SharedLogic.Allocator.Jobs.SharedCab.Reconciler
  ( runSharedCabReconcilerJob,
    seedSharedCabReconcilerChain,
    sharedCabReconcilerSeededKey,
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HMS
import qualified Data.Map as M
import qualified Domain.Types.DriverInformation as DDI
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import EulerHS.Types (EulerClient, client)
import Kernel.Beam.Functions (findAllWithKVScheduler)
import Kernel.Beam.Lib.UtilsTH (HasSchemaName)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error (GenericError (InternalError))
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Error.BaseError.HTTPError.APIError (APICallError (..))
import qualified Kernel.Utils.Servant.Client as EC
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import Lib.Scheduler
import Lib.Scheduler.JobStorageType.DB.Table (SchedulerJobT)
import qualified Lib.Scheduler.JobStorageType.DB.Table as SJT
import qualified Lib.Scheduler.JobStorageType.SchedulerType as JC
import Servant hiding (throwError)
import Sequelize as Se
import SharedLogic.Allocator (AllocatorJobType (SharedCabReconciler), SharedCabReconcilerJobData (..))
import SharedLogic.CallBAPInternal (AppBackendBapInternal)
import qualified SharedLogic.SharedCab.Flag as SharedCabFlag
import Storage.Beam.SchedulerJob ()
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.DriverInformationExtra as QDIExtra
import qualified Storage.Queries.Vehicle as QVeh
import Tools.Metrics (CoreMetrics)

-- Fixed sweep interval; per-city on/off lives on TransporterConfig itself, so
-- cadence stays a code constant for the pilot (make it config if ops asks).
sharedCabReconcilerInterval :: NominalDiffTime
sharedCabReconcilerInterval = 900 -- 15 minutes

sharedCabReconcilerBatchSize :: Int
sharedCabReconcilerBatchSize = 100

-- | The unambiguous end signals, verbatim from the rider-app's
--   Tools/Error.hs SharedCabSessionError 'toErrorCode':
--     * NOT_FOUND: no session key exists, or it exists but is not live
--       (ENDED/PAUSED-expired) — the session the flag belongs to is gone.
--     * HELD_BY_ANOTHER_DRIVER: a DIFFERENT driverId owns the live session
--       on this vehicle, so this driver's flag is stale by construction.
--   Both are thrown by the rider-app session handler itself, so decoding
--   this errorCode is the 'NOT_FOUND'-family the fail-closed contract
--   accepts; a bare status code is never enough.
clearSignalErrorCodes :: [Text]
clearSignalErrorCodes = ["SHARED_CAB_SESSION_NOT_FOUND", "SHARED_CAB_SESSION_HELD_BY_ANOTHER_DRIVER"]

-- GET /internal/sharedCab/session?driverId=&vehicleNumber= --------------------
-- Mirrors SharedLogic.CallSharedCabBAP.getSharedCabSession (45 branch), but
-- self-contained so this branch doesn't need the proxy client module: the
-- response payload is opaque (we only need success vs the error code), hence
-- Value.

type SharedCabSessionAPI =
  "internal"
    :> "sharedCab"
    :> "session"
    :> QueryParam' '[Required, Strict] "driverId" Text
    :> QueryParam' '[Required, Strict] "vehicleNumber" Text
    :> Header "token" Text
    :> Get '[JSON] Aeson.Value

callSessionClient :: Text -> Text -> Maybe Text -> EulerClient Aeson.Value
callSessionClient = client (Proxy @SharedCabSessionAPI)

callSessionAPI :: Proxy SharedCabSessionAPI
callSessionAPI = Proxy

-- | What one session probe decided for one driver. Exception outcomes are
--   handled in the job's per-driver wrapper, not here.
data SessionProbe
  = -- | Clear, with the decoded errorCode for the audit log line.
    SessionGone Text
  | -- | No unambiguous end signal — leave the flag untouched.
    SessionUnknown
  deriving (Eq, Show)

-- | Probe the rider-app for this driver's live session. Uses the kernel
--   'APICallError' wrapper ('callApiUnwrappingApiError' with the kernel
--   APIError FromResponse decoder): a failure response whose body is the
--   rider-app's JSON error contract decodes to a typed APICallError carrying
--   the errorCode straight through; every other failure (bare 404s, HTML
--   5xx, connection errors, timeouts, undecodable bodies) surfaces as
--   ExternalAPICallError — deliberately NOT a typed signal, so it lands on
--   'SessionUnknown'.
probeSharedCabSession ::
  ( MonadFlow m,
    CoreMetrics m,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HMS.HashMap BaseUrl BaseUrl],
    HasRequestId r
  ) =>
  Text ->
  BaseUrl ->
  Text ->
  Text ->
  m SessionProbe
probeSharedCabSession apiKey internalUrl driverId vehicleNumber = do
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  eResp <-
    withTryCatch "getSharedCabSession:sharedCabReconciler" $
      EC.callApiUnwrappingApiError APICallError Nothing (Just "BAP_INTERNAL_API_ERROR") (Just internalEndPointHashMap) internalUrl (callSessionClient driverId vehicleNumber (Just apiKey)) "GetSharedCabSession" callSessionAPI
  pure $ case eResp of
    Right _ -> SessionUnknown -- 2xx: a live session answered; nothing to do
    Left exc
      | Just (APICallError apiErr) <- fromException @APICallError exc,
        apiErr.errorCode `elem` clearSignalErrorCodes ->
        SessionGone apiErr.errorCode
      | otherwise -> SessionUnknown -- everything else: fail-closed, keep the flag

runSharedCabReconcilerJob ::
  forall m r c.
  ( EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    Redis.HedisLTSFlowEnv r,
    MonadFlow m,
    MonadIO m,
    CoreMetrics m,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HMS.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["appBackendBapInternal" ::: AppBackendBapInternal],
    HasShortDurationRetryCfg r c,
    HasField "maxShards" r Int,
    HasField "schedulerSetName" r Text,
    HasField "schedulerType" r SchedulerType,
    HasField "jobInfoMap" r (M.Map Text Bool),
    HasField "blackListedJobs" r [Text],
    JobCreatorEnv r,
    HasSchemaName SchedulerJobT
  ) =>
  Job 'SharedCabReconciler ->
  m ExecutionResult
runSharedCabReconcilerJob Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) $ do
  let jobData :: SharedCabReconcilerJobData = jobInfo.jobData
      merchantId = jobData.merchantId
      merchantOpCityId = jobData.merchantOperatingCityId
  mbTc <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId}) Nothing
  let reconcilerOn = maybe False (fromMaybe False . (.sharedCabReconcilerEnabled)) mbTc
  if not reconcilerOn
    then do
      logInfo $ "SharedCabReconciler disabled for city " <> merchantOpCityId.getId <> "; dropping job chain"
      -- R26: the chain is INTENTIONALLY ending here — release the seed marker
      -- so a later trigger seed is allowed to start a fresh chain for this
      -- city. Best-effort: a DEL failure must not throw (that would kill the
      -- chain silently AND leave the marker), so we log the exact key for
      -- manual cleanup instead.
      eRelease <- withTryCatch "sharedCabReconciler:releaseSeedMarker" $ releaseSharedCabReconcilerSeedMarker merchantOpCityId
      case eRelease of
        Right () -> pure ()
        Left err ->
          logError $ "SharedCabReconciler: failed to release seed marker for city " <> merchantOpCityId.getId <> ": " <> show err <> "; ops must DEL " <> sharedCabReconcilerSeededKey merchantOpCityId <> " (cross-app key) before reseeding"
      pure Complete
    else do
      bap <- asks (.appBackendBapInternal)
      -- The sweep is exception-insulated so the re-enqueue below ALWAYS
      -- runs: a partial failure (a single bad query, a BAP outage) must not
      -- kill the scheduler chain for the city.
      eSweep <- withTryCatch "sharedCabReconciler:sweep" $ reconcileAll merchantOpCityId bap.apiKey bap.url Nothing (0 :: Int, 0 :: Int, 0 :: Int)
      case eSweep of
        Right (checked, cleared, leftFlag) ->
          logInfo $ "SharedCabReconciler city=" <> merchantOpCityId.getId <> " checked=" <> show (checked :: Int) <> " cleared=" <> show (cleared :: Int) <> " left=" <> show (leftFlag :: Int)
        Left err ->
          logError $ "SharedCabReconciler city=" <> merchantOpCityId.getId <> " sweep aborted: " <> show err <> "; re-enqueueing, un-reconciled flags stay as they were"
      JC.createJobIn @_ @'SharedCabReconciler (Just merchantId) (Just merchantOpCityId) sharedCabReconcilerInterval jobData
      pure Complete
  where
    -- One full keyset pass over the city's flagged drivers (batch_size-ordered
    -- by driverId; each page resumes after the last driverId of the previous
    -- page, so clearing a row cannot shift the walk). A row that flips True
    -- again mid-sweep is harmless: the next fire rechecks it.
    -- Counts: checked = probed via the BAP, cleared = flag flipped False,
    -- left = kept True (live session true positive or any ambiguity).
    reconcileAll :: Id DMOC.MerchantOperatingCity -> Text -> BaseUrl -> Maybe (Id DP.Person) -> (Int, Int, Int) -> m (Int, Int, Int)
    reconcileAll merchantOpCity apiKey internalUrl mbLastSeen (checked, cleared, leftFlag) = do
      flagged <- QDIExtra.findSharedCabActiveDriversAfter merchantOpCity mbLastSeen sharedCabReconcilerBatchSize
      case flagged of
        [] -> pure (checked, cleared, leftFlag)
        batch -> do
          (cleared', leftFlag') <- foldM (reconcileDriver apiKey internalUrl) (0 :: Int, 0 :: Int) batch
          reconcileAll merchantOpCity apiKey internalUrl (Just (last batch).driverId) (checked + length batch, cleared + cleared', leftFlag + leftFlag')

    reconcileDriver :: Text -> BaseUrl -> (Int, Int) -> DDI.DriverInformation -> m (Int, Int)
    reconcileDriver apiKey internalUrl (cleared, leftFlag) driverInfo = do
      let driverId = driverInfo.driverId
      mbVehicle <- QVeh.findById driverId
      case mbVehicle of
        Nothing -> do
          -- No plate = the probe can not even be formed; this is a flagged
          -- row with no vehicle at all, an anomaly for ops — NOT an end
          -- signal from the rider-app, so fail-closed keeps the flag.
          logError $ "SharedCabReconciler: driver " <> driverId.getId <> " flagged but has no vehicle; leaving flag set"
          pure (cleared, leftFlag + 1)
        Just vehicle -> do
          eProbe <- withTryCatch "sharedCabReconciler:probe" $ probeSharedCabSession apiKey internalUrl driverId.getId vehicle.registrationNo
          case eProbe of
            Right (SessionGone signal) -> do
              logWarning $ "SharedCabReconciler: clearing flag for driver " <> driverId.getId <> " on decoded signal " <> signal
              clearFlag driverId
              pure (cleared + 1, leftFlag)
            Right SessionUnknown -> do
              logDebug $ "SharedCabReconciler: no clear signal for driver " <> driverId.getId <> "; leaving flag"
              pure (cleared, leftFlag + 1)
            Left err -> do
              logWarning $ "SharedCabReconciler: probe failed for driver " <> driverId.getId <> ": " <> show err <> "; leaving flag"
              pure (cleared, leftFlag + 1)

    -- R26 (b): the clear path delegates to the single exported wrapper —
    -- SharedLogic.SharedCab.Flag, the ONLY caller of QDIExtra
    -- .updateSharedCabSessionActive (cross-app master cell write).
    clearFlag :: Id DP.Person -> m ()
    clearFlag driverId = SharedCabFlag.clearSharedCabSessionActive driverId


-- DUPLICATE-CHAIN GUARD (R26) ---------------------------------------------------
-- Seed path for the FIRST job of a city's chain; the only legal caller is the
-- dashboard scheduler trigger (postMerchantSchedulerTrigger). Guard semantics
-- are documented in the module header above.

-- | Raw cross-app Redis key: "a reconciler chain for this city was seeded".
--   No TTL: it mirrors the intended chain state, released only when the chain
--   intentionally ends (reconciler disabled) or an in-flight seed fails. Main
--   (dashboard trigger) SETNXes it, the Allocator scheduler DELs it — both
--   under withCrossAppRedis because the two services run with different
--   hedis key prefixes ("dynamic-offer-driver-app:" vs
--   "driver-offer-scheduler:"); cross-app removes the modifier, and the
--   master-cell wrapper keeps both cells (primary/secondary) consistent.
sharedCabReconcilerSeededKey :: Id DMOC.MerchantOperatingCity -> Text
sharedCabReconcilerSeededKey merchantOpCityId = "sharedcab:reconciler:seeded:" <> merchantOpCityId.getId

-- | Release the seed marker for the city (see 'sharedCabReconcilerSeededKey').
releaseSharedCabReconcilerSeedMarker :: Redis.HedisFlow m r => Id DMOC.MerchantOperatingCity -> m ()
releaseSharedCabReconcilerSeedMarker merchantOpCityId =
  Redis.runInMasterCloudRedisCellWithCrossAppRedis $
    Redis.del (sharedCabReconcilerSeededKey merchantOpCityId)

-- | Existing chain roots: scheduler_job rows of this job type for this city
--   whose status is Pending — a fired job marks itself Completed and the
--   handler creates the next Pending row, so a live chain always holds
--   exactly one Pending row; Completed/Failed rows are dead roots and must
--   NOT count (module header, R26 STEP-0 note).
findLiveSharedCabReconcilerJobs ::
  ( EsqDBFlow m r,
    MonadFlow m
  ) =>
  Id DMOC.MerchantOperatingCity ->
  m [AnyJob AllocatorJobType]
findLiveSharedCabReconcilerJobs merchantOpCityId =
  findAllWithKVScheduler
    [ Se.And
        [ Se.Is SJT.status $ Se.Eq Pending,
          Se.Is SJT.jobType $ Se.Eq (show SharedCabReconciler),
          Se.Is SJT.merchantOperatingCityId $ Se.Eq (Just merchantOpCityId.getId)
        ]
    ]

-- | Seed the reconciler chain for a city, idempotently. Order matters:
--   DB-EXISTENCE first (cheap, durable), then the SETNX (atomic race guard),
--   then the job creation. A concurrent pair of seeds interleaves so that
--   at most one SETNX ever wins; the loser sees either the winner's Pending
--   row (DbBased deployments) or the marker (always) and creates nothing.
seedSharedCabReconcilerChain ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    MonadFlow m,
    CoreMetrics m,
    JobCreator r m,
    HasSchemaName SchedulerJobT
  ) =>
  Maybe (Id DM.Merchant) ->
  Id DMOC.MerchantOperatingCity ->
  NominalDiffTime ->
  SharedCabReconcilerJobData ->
  m ()
seedSharedCabReconcilerChain mbMerchantId merchantOpCityId diffTimeS jobData = do
  liveJobs <- findLiveSharedCabReconcilerJobs merchantOpCityId
  if not (null liveJobs)
    then
      logInfo $ "SharedCabReconciler: a live chain root (scheduler_job status=Pending) already exists for city " <> merchantOpCityId.getId <> "; skipping duplicate seed"
    else do
      firstSeed <- Redis.runInMasterCloudRedisCellWithCrossAppRedis $ Redis.setNx (sharedCabReconcilerSeededKey merchantOpCityId) True
      if not firstSeed
        then
          logInfo $ "SharedCabReconciler: seed marker already set for city " <> merchantOpCityId.getId <> "; skipping duplicate seed"
        else do
          eCreate <- withTryCatch "sharedCabReconciler:seed:createJob" $ JC.createJobIn @_ @'SharedCabReconciler mbMerchantId (Just merchantOpCityId) diffTimeS jobData
          case eCreate of
            Right () ->
              logInfo $ "SharedCabReconciler: seeded chain for city " <> merchantOpCityId.getId
            Left err -> do
              logError $ "SharedCabReconciler: seed failed for city " <> merchantOpCityId.getId <> ": " <> show err
              -- release the marker so the next trigger can reseed; the seed
              -- itself fails loudly (never a silent half-seeded state)
              eRelease <- withTryCatch "sharedCabReconciler:seed:releaseMarker" $ releaseSharedCabReconcilerSeedMarker merchantOpCityId
              case eRelease of
                Right () -> pure ()
                Left delErr -> logError $ "SharedCabReconciler: failed to release seed marker for city " <> merchantOpCityId.getId <> ": " <> show delErr <> "; ops must DEL " <> sharedCabReconcilerSeededKey merchantOpCityId <> " (cross-app key) before reseeding"
              throwError $ InternalError ("SharedCabReconciler seed failed for city " <> merchantOpCityId.getId)
