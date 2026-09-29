-- | Shared-cab reconciler chain: seeding + dead-chain recovery (R26).
--
--   The reconciler chain ('SharedLogic.Allocator.Jobs.SharedCab.Reconciler')
--   is one self-re-enqueuing scheduler job per (merchant, city). Its FIRST
--   job is created here, and only here — legal callers:
--
--     * BOOT SEEDER: the dashboard scheduler trigger
--       (Domain.Action.Dashboard.Management.Merchant
--       .postMerchantSchedulerTrigger, SharedCabReconcilerTrigger branch),
--       run on boot/deploy or by ops;
--
--     * SELECT-ROUTE SEEDER (boot-INDEPENDENT recovery):
--       Domain.Action.UI.SharedCab.selectSharedCabRoute re-runs the cheap
--       idempotent seeder on every shared-cab select — the hottest driver
--       action in the launch city — fire-and-forget, so a driver call can
--       not be blocked by a seed failure and a dead chain recovers between
--       deploys, not just at the next one.
--
--   DUPLICATE-CHAIN GUARD ('seedSharedCabReconcilerChain'):
--
--     1. DB-EXISTENCE: a scheduler_job row of jobType SharedCabReconciler
--        for this city with status Pending means a chain is alive and the
--        seed is skipped. ONLY Pending counts — Completed/Failed rows are
--        dead chain roots and must never block a reseed. NOTE (claude's
--        STEP-0 check, verified in R26): driver-app runs the RedisBased
--        scheduler (dhall schedulerType) and SharedCabReconciler is NOT in
--        jobInfoMap, so under the deployed config job records live only in
--        Redis (zset + stream), the Redis-side lookup fns are stubs that
--        return [], and the table check is VACUOUS — the SETNX below is the
--        de-facto primary guard; the DB check becomes the durable truth the
--        day the job is marked long-running or the scheduler flips to
--        DbBased.
--     2. SETNX-WITH-TTL on sharedcab:reconciler:seeded:<city> (raw cross-app
--        key in the master cell — Main and the Allocator scheduler use
--        different hedis key prefixes, so the key goes through
--        withCrossAppRedis to strip them): one ATOMIC SET ... NX EX (hedis
--        SetOpts with EX + NX in one command — the library HAS the atomic
--        op, no SETNX+EXPIRE pair needed). The raw reply is matched (claude
--        MED-2): Ok = won, nil = the marker exists, ANYTHING ELSE is a LOUD
--        'InternalError' — kernel setNx/setNxExpire fold a Redis error into
--        False, which would read as "already seeded" and leave the city
--        silently reconciler-less. Loser skips; winner creates the first
--        job. If createJob fails after winning, the marker is released again
--        so a later seed can retry, and the seed call fails loudly instead
--        of returning a false Success. Every outcome SKIPs are reported as
--        'SharedCabSeedOutcome' (claude MED-3), never as a creation success.
--
--   DEAD-CHAIN RECOVERY CONTRACT (heartbeat refresh):
--
--     * The marker carries TTL = 3 x 'sharedCabReconcilerInterval' (the
--       chain's fire/re-enqueue interval — the cadence the job itself uses;
--       see 'sharedCabReconcilerSeedMarkerTtlSeconds' for the derivation —
--       NO magic literal).
--     * Every fire of the chain REFRESHES the marker (plain SET ... EX, no
--       NX — overwrites a live value) BEFORE any enabled/disabled branching,
--       so while the chain exists at all the marker can never expire.
--     * If the chain DIES (lost job, crash between fire and re-enqueue,
--       stream trim, Redis flush), no more refreshes arrive and Redis
--       expires the marker within one TTL. The NEXT recovery hook — boot
--       seeder OR select-route seeder — then passes step 2 and recreates
--       EXACTLY ONE chain (SETNX still arbitrates concurrent recoveries).
--     * DETECTION LATENCY = marker TTL + the next boot/deploy OR the next
--       shared-cab select-route call, whichever comes first. There is NO
--       autonomous restart: nothing runs between hooks.
--     * Tolerates up to 2 consecutive failed refreshes per fire cycle
--       (TTL 3x the interval) before a live chain's marker can lapse.
--
--   When the chain INTENTIONALLY ends (reconciler disabled for the city) the
--   marker is RELEASED (DEL) instead of left to expire, so ops can re-enable
--   and reseed immediately. A full Redis flush loses chain + marker
--   together, so the next hook reseeds exactly one clean chain.
module SharedLogic.SharedCab.ReconcilerSeed
  ( sharedCabReconcilerInterval,
    sharedCabReconcilerSeedMarkerTtlSeconds,
    sharedCabReconcilerSeededKey,
    refreshSharedCabReconcilerSeedMarker,
    releaseSharedCabReconcilerSeedMarker,
    SharedCabSeedOutcome (..),
    seedSharedCabReconcilerChain,
    trySeedSharedCabReconcilerChain,
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Database.Redis as Hedis
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Kernel.Beam.Functions (findAllWithKVScheduler)
import Kernel.Beam.Lib.UtilsTH (HasSchemaName)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error (GenericError (InternalError))
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Scheduler
import Lib.Scheduler.JobStorageType.DB.Table (SchedulerJobT)
import qualified Lib.Scheduler.JobStorageType.DB.Table as SJT
import qualified Lib.Scheduler.JobStorageType.SchedulerType as JC
import Sequelize as Se
import SharedLogic.Allocator (AllocatorJobType (SharedCabReconciler), SharedCabReconcilerJobData)
import Storage.Beam.SchedulerJob ()
import Tools.Metrics (CoreMetrics)

-- | Fixed sweep interval; per-city on/off lives on TransporterConfig itself, so
--   cadence stays a code constant for the pilot (make it config if ops asks).
--   The chain re-enqueues every fire with exactly this interval — the "fire
--   interval the job config uses".
sharedCabReconcilerInterval :: NominalDiffTime
sharedCabReconcilerInterval = 900 -- 15 minutes

-- | Marker TTL, DERIVED from the fire interval above: the marker must outlive
--   up to two missed/failed refreshes (three fire intervals) while the chain
--   is live, and expire promptly once it is dead. 3 x 900s = 2700s = 45min.
sharedCabReconcilerSeedMarkerTtlSeconds :: Redis.ExpirationTime
sharedCabReconcilerSeedMarkerTtlSeconds = ceiling (3 * sharedCabReconcilerInterval)

-- | Raw cross-app Redis key: "a reconciler chain for this city was seeded".
--   Carries TTL (see module header): refreshed SET ... EX on every chain
--   fire, released (DEL) when the chain intentionally ends (reconciler
--   disabled) or an in-flight seed fails. Main (boot trigger) and any driver
--   UI call SETNX it, the Allocator scheduler refreshes/DELs it — all under
--   withCrossAppRedis because the services run with different hedis key
--   prefixes ("dynamic-offer-driver-app:" vs "driver-offer-scheduler:");
--   cross-app removes the modifier, and the master-cell wrapper keeps both
--   cells (primary/secondary) consistent.
sharedCabReconcilerSeededKey :: Id DMOC.MerchantOperatingCity -> Text
sharedCabReconcilerSeededKey merchantOpCityId = "sharedcab:reconciler:seeded:" <> merchantOpCityId.getId

-- | Heartbeat refresh, called once per chain fire: SET ... EX (no NX — a
--   still-live value is simply overwritten) with the marker TTL, pushing the
--   expiry three fire-intervals into the future. Best-effort by construction
--   ('Redis.setExp' logs Redis failures internally), and callers additionally
--   protect it so a refresh hiccup can never kill the chain it marks.
refreshSharedCabReconcilerSeedMarker :: Redis.HedisFlow m r => Id DMOC.MerchantOperatingCity -> m ()
refreshSharedCabReconcilerSeedMarker merchantOpCityId =
  Redis.runInMasterCloudRedisCellWithCrossAppRedis $
    Redis.setExp (sharedCabReconcilerSeededKey merchantOpCityId) True sharedCabReconcilerSeedMarkerTtlSeconds

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

-- | What one seed attempt decided (claude MED-3): a SKIPPED seed is NOT a
--   creation success and MUST NOT be reported as one — the outcome travels
--   with the call so the boot trigger and the hot-path hook can each surface
--   it (the trigger throws 'InvalidRequest' on a skip; the fire-and-forget
--   hook logs it).
data SharedCabSeedOutcome
  = -- | This call won the race and created the first job of the chain.
    SharedCabChainSeeded
  | -- | DB-EXISTENCE: a Pending scheduler_job row for the city says a chain
    --   is live (durable truth under DbBased; vacuous under the deployed
    --   RedisBased config).
    SharedCabSeedSkippedLiveRow
  | -- | The live seed marker blocked the seed (a chain exists, or a dead
    --   chain's marker has not yet expired — recovery then happens at most
    --   one marker TTL later).
    SharedCabSeedSkippedMarker
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Seed the reconciler chain for a city, idempotently. Order matters:
--   DB-EXISTENCE first (cheap, durable), then the atomic SETNX-WITH-TTL
--   (race guard; the TTL is what bounds dead-chain recovery), then the job
--   creation. A concurrent pair of seeds interleaves so that at most one
--   SETNX ever wins; the loser sees either the winner's Pending row (DbBased
--   deployments) or the marker (always) and creates nothing.
--
--   ERRORS ARE LOUD, never folded into a skip (claude MED-2): a Redis error
--   during the marker probe or a createJob failure throws 'InternalError'
--   (plus 'logError'), so the boot trigger retries instead of silently
--   leaving the city without a reconciler; the fire-and-forget UI hook
--   swallows the throw and simply retries on the next call.
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
  m SharedCabSeedOutcome
seedSharedCabReconcilerChain mbMerchantId merchantOpCityId diffTimeS jobData = do
  liveJobs <- findLiveSharedCabReconcilerJobs merchantOpCityId
  if not (null liveJobs)
    then do
      logInfo $ "SharedCabReconciler: a live chain root (scheduler_job status=Pending) already exists for city " <> merchantOpCityId.getId <> "; seed SKIPPED (live row)"
      pure SharedCabSeedSkippedLiveRow
    else do
      -- One ATOMIC command: SET key val EX ttl NX — the marker is born with
      -- its TTL, there is no SETNX-then-EXPIRE window. The probe must NOT go
      -- through kernel 'Redis.setNx'/'Redis.setNxExpire': they flatten a
      -- Redis ERROR reply to False (claude MED-2), indistinguishable from
      -- "marker exists" — a Redis blip would silently leave the city
      -- reconciler-less while the caller reported success. The raw hedis
      -- reply is matched instead: Right Ok = the SET won; Bulk Nothing =
      -- Redis nil, the genuine NX condition failure (marker exists);
      -- anything else (an Error reply, or an exception from the connection
      -- underneath) is a LOUD failure.
      eProbe <-
        withTryCatch "sharedCabReconciler:seed:setMarker" $
          Redis.runInMasterCloudRedisCellWithCrossAppRedis $
            Redis.runWithPrefixEither (sharedCabReconcilerSeededKey merchantOpCityId) $ \prefKey ->
              Hedis.setOpts prefKey (BSL.toStrict $ Aeson.encode True) $
                Hedis.SetOpts (Just $ toInteger sharedCabReconcilerSeedMarkerTtlSeconds) Nothing (Just Hedis.Nx)
      case eProbe of
        Right (Right Hedis.Ok) -> do
          eCreate <- withTryCatch "sharedCabReconciler:seed:createJob" $ JC.createJobIn @_ @'SharedCabReconciler mbMerchantId (Just merchantOpCityId) diffTimeS jobData
          case eCreate of
            Right () -> do
              logInfo $ "SharedCabReconciler: seeded chain for city " <> merchantOpCityId.getId
              pure SharedCabChainSeeded
            Left err -> do
              logError $ "SharedCabReconciler: seed failed for city " <> merchantOpCityId.getId <> ": " <> show err
              -- release the marker so the next hook can reseed immediately
              -- (not only after TTL expiry); the seed itself fails loudly
              -- (never a silent half-seeded state)
              eRelease <- withTryCatch "sharedCabReconciler:seed:releaseMarker" $ releaseSharedCabReconcilerSeedMarker merchantOpCityId
              case eRelease of
                Right () -> pure ()
                Left delErr -> logError $ "SharedCabReconciler: failed to release seed marker for city " <> merchantOpCityId.getId <> ": " <> show delErr <> "; ops must DEL " <> sharedCabReconcilerSeededKey merchantOpCityId <> " (cross-app key) before reseeding"
              throwError $ InternalError ("SharedCabReconciler seed failed for city " <> merchantOpCityId.getId)
        Right (Right status) -> do
          -- a successful command that answered anything but OK (Pong/Status
          -- never happens for SET ... NX EX, but never read it as 'exists')
          logError $ "SharedCabReconciler: seed marker probe for city " <> merchantOpCityId.getId <> " replied " <> show status <> " instead of OK; NOT treating it as 'already seeded' — failing loudly so the next hook retries"
          throwError $ InternalError ("SharedCabReconciler seed marker probe failed for city " <> merchantOpCityId.getId)
        Right (Left (Hedis.Bulk Nothing)) -> do
          logInfo $ "SharedCabReconciler: seed marker already set for city " <> merchantOpCityId.getId <> "; seed SKIPPED (marker)"
          pure SharedCabSeedSkippedMarker
        Right (Left reply) -> do
          logError $ "SharedCabReconciler: seed marker probe for city " <> merchantOpCityId.getId <> " got a Redis error reply " <> show reply <> "; NOT treating it as 'already seeded' — failing loudly so the next hook retries"
          throwError $ InternalError ("SharedCabReconciler seed marker probe failed for city " <> merchantOpCityId.getId)
        Left err -> do
          logError $ "SharedCabReconciler: seed marker probe for city " <> merchantOpCityId.getId <> " failed: " <> show err <> "; NOT treating it as 'already seeded' — failing loudly so the next hook retries"
          throwError $ InternalError ("SharedCabReconciler seed marker probe failed for city " <> merchantOpCityId.getId)

-- | Fire-and-forget seeder for HOT code paths (the select-route recovery
--   hook): wraps 'seedSharedCabReconcilerChain' so that ANY failure — a
--   Redis hiccup, a createJob failure — is logged and swallowed, never
--   propagated into the driver's request. While the marker is alive the
--   whole seed is a cheap no-op (one marker probe; the DB check is skipped
--   only under DbBased), so running it on every shared-cab select costs one
--   extra Redis command per call in the steady state.
trySeedSharedCabReconcilerChain ::
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
  SharedCabReconcilerJobData ->
  m ()
trySeedSharedCabReconcilerChain mbMerchantId merchantOpCityId jobData = do
  eSeed <- withTryCatch "sharedCabReconciler:fireAndForgetSeed" $ seedSharedCabReconcilerChain mbMerchantId merchantOpCityId sharedCabReconcilerInterval jobData
  case eSeed of
    Right outcome -> logInfo $ "SharedCabReconciler: opportunistic seed check done for city " <> merchantOpCityId.getId <> " outcome=" <> show outcome
    Left err -> logInfo $ "SharedCabReconciler: opportunistic seed check failed for city " <> merchantOpCityId.getId <> " (will retry on the next hook): " <> show err
