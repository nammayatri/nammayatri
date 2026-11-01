-- | Shared-cab reconciler chain: seeding and dead-chain recovery.
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

sharedCabReconcilerInterval :: NominalDiffTime
sharedCabReconcilerInterval = 900 -- 15 minutes

-- | Marker TTL: three fire intervals (3 x 900s), so it outlives two missed refreshes while live and expires promptly once dead.
sharedCabReconcilerSeedMarkerTtlSeconds :: Redis.ExpirationTime
sharedCabReconcilerSeedMarkerTtlSeconds = ceiling (3 * sharedCabReconcilerInterval)

-- | Raw cross-app Redis key (Main and the Allocator scheduler use different hedis prefixes, hence withCrossAppRedis in the master cell).
sharedCabReconcilerSeededKey :: Id DMOC.MerchantOperatingCity -> Text
sharedCabReconcilerSeededKey merchantOpCityId = "sharedcab:reconciler:seeded:" <> merchantOpCityId.getId

-- | Heartbeat refresh per chain fire (SET ... EX, no NX). Best-effort: callers protect it so it can never kill the chain.
refreshSharedCabReconcilerSeedMarker :: Redis.HedisFlow m r => Id DMOC.MerchantOperatingCity -> m ()
refreshSharedCabReconcilerSeedMarker merchantOpCityId =
  Redis.runInMasterCloudRedisCellWithCrossAppRedis $
    Redis.setExp (sharedCabReconcilerSeededKey merchantOpCityId) True sharedCabReconcilerSeedMarkerTtlSeconds

releaseSharedCabReconcilerSeedMarker :: Redis.HedisFlow m r => Id DMOC.MerchantOperatingCity -> m ()
releaseSharedCabReconcilerSeedMarker merchantOpCityId =
  Redis.runInMasterCloudRedisCellWithCrossAppRedis $
    Redis.del (sharedCabReconcilerSeededKey merchantOpCityId)

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

data SharedCabSeedOutcome
  = -- | This call won the race and created the first job of the chain.
    SharedCabChainSeeded
  | -- | DB-EXISTENCE: a Pending scheduler_job row for the city says a chain
    SharedCabSeedSkippedLiveRow
  | -- | The live seed marker blocked the seed (a chain exists, or a dead
    SharedCabSeedSkippedMarker
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Seed idempotently: DB-existence check, then one atomic SET ... NX EX on the marker (the race guard; its TTL bounds dead-chain
-- recovery; under RedisBased the DB check is vacuous), then job creation. Errors are loud, never a skip, or a city is silently reconciler-less.
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
      -- One ATOMIC command (no SETNX-then-EXPIRE window). Not kernel setNx, which flattens a Redis ERROR to False ("marker exists"): match the raw reply.
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
              eRelease <- withTryCatch "sharedCabReconciler:seed:releaseMarker" $ releaseSharedCabReconcilerSeedMarker merchantOpCityId
              case eRelease of
                Right () -> pure ()
                Left delErr -> logError $ "SharedCabReconciler: failed to release seed marker for city " <> merchantOpCityId.getId <> ": " <> show delErr <> "; ops must DEL " <> sharedCabReconcilerSeededKey merchantOpCityId <> " (cross-app key) before reseeding"
              throwError $ InternalError ("SharedCabReconciler seed failed for city " <> merchantOpCityId.getId)
        Right (Right status) -> do
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

-- | Fire-and-forget seeder for hot paths: any failure is logged and swallowed, never propagated into the driver's request.
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
