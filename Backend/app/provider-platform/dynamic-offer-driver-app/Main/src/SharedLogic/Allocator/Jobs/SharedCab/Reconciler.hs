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
--   DUPLICATE-CHAIN GUARD + DEAD-CHAIN RECOVERY (R26): every seed goes
--   through 'SharedLogic.SharedCab.ReconcilerSeed.seedSharedCabReconcilerChain'
--   (DB-existence, then one ATOMIC SETNX-WITH-TTL on the cross-app marker
--   key), so a re-trigger, redeploy or hot-path retry can not start a second
--   self-re-enqueuing chain for the same city. DEAD-CHAIN RECOVERY: the
--   marker carries TTL = 3 x the fire interval and EVERY FIRE of this job
--   refreshes it (below, before the enabled check, so even a disabled fire
--   refreshes while the chain exists); a dead chain stops refreshing, the
--   marker self-expires, and the next recovery hook (boot seeder or
--   select-route seeder) recreates exactly one chain. Detection latency =
--   marker TTL + the next hook. The full contract lives in the module
--   header of SharedLogic.SharedCab.ReconcilerSeed.
--
--   The chain's own re-enqueue (below, after every fire) is deliberately
--   NOT guarded: a fire is by definition inside the live chain. When the
--   chain INTENTIONALLY ends (reconciler disabled for the city) the
--   marker is released so ops can re-enable and reseed later; a full
--   Redis flush loses chain + marker together, so the next hook then
--   reseeds exactly one clean chain.
module SharedLogic.Allocator.Jobs.SharedCab.Reconciler
  ( runSharedCabReconcilerJob,
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HMS
import qualified Data.Map as M
import qualified Domain.Types.DriverInformation as DDI
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import EulerHS.Types (EulerClient, client)
import Kernel.Beam.Lib.UtilsTH (HasSchemaName)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Error.BaseError.HTTPError.APIError (APICallError (..))
import qualified Kernel.Utils.Servant.Client as EC
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import Lib.Scheduler
import Lib.Scheduler.JobStorageType.DB.Table (SchedulerJobT)
import qualified Lib.Scheduler.JobStorageType.SchedulerType as JC
import Servant hiding (throwError)
import SharedLogic.Allocator (AllocatorJobType (SharedCabReconciler), SharedCabReconcilerJobData (..))
import SharedLogic.CallBAPInternal (AppBackendBapInternal)
import qualified SharedLogic.SharedCab.Flag as SharedCabFlag
import qualified SharedLogic.SharedCab.ReconcilerSeed as SharedCabSeed
import Storage.Beam.SchedulerJob ()
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.DriverInformationExtra as QDIExtra
import qualified Storage.Queries.Vehicle as QVeh
import Tools.Metrics (CoreMetrics)

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
  -- R26 dead-chain recovery: HEARTBEAT REFRESH — every fire re-arms the seed
  -- marker's TTL (SET ... EX, no NX: overwrites a still-live value on the same
  -- raw cross-app master-cell key). Placed BEFORE the enabled check so it
  -- runs while the chain exists at all; a dead chain stops refreshing, the
  -- marker expires within 3 fire intervals, and the next recovery hook
  -- (boot or select-route seeder) recreates exactly one chain. When the
  -- chain ends intentionally below, the marker is released as before.
  -- Best-effort: a refresh failure must not kill the chain it marks.
  eRefresh <- withTryCatch "sharedCabReconciler:refreshSeedMarker" $ SharedCabSeed.refreshSharedCabReconcilerSeedMarker merchantOpCityId
  case eRefresh of
    Right () -> pure ()
    Left err -> logError $ "SharedCabReconciler: seed marker refresh failed for city " <> merchantOpCityId.getId <> ": " <> show err
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
      eRelease <- withTryCatch "sharedCabReconciler:releaseSeedMarker" $ SharedCabSeed.releaseSharedCabReconcilerSeedMarker merchantOpCityId
      case eRelease of
        Right () -> pure ()
        Left err ->
          logError $ "SharedCabReconciler: failed to release seed marker for city " <> merchantOpCityId.getId <> ": " <> show err <> "; ops must DEL " <> SharedCabSeed.sharedCabReconcilerSeededKey merchantOpCityId <> " (cross-app key) before reseeding"
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
      JC.createJobIn @_ @'SharedCabReconciler (Just merchantId) (Just merchantOpCityId) SharedCabSeed.sharedCabReconcilerInterval jobData
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
