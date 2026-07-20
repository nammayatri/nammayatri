-- | Per-chunk auto-schedule wrapper for a single recon spec.
--
--   Fires at the recipe's own chunk cadence. On each invocation it:
--
--     1. Looks up the recipe via 'reconciliationRegistry' to read
--        'settlementBuffer' and 'chunkPlan'.
--     2. Computes the latest chunk whose exclusive end is at or before
--        @now - settlementBuffer@ (rounded down to the plan's natural
--        grid so successive fires never overlap or gap).
--     3. Enqueues an immediate 'Reconciliation' job for that one chunk.
--     4. Re-enqueues itself at @now + chunkDuration@ so the cadence
--        matches the recipe's plan (hourly for ByHour, daily for ByDay).
--
--   Fire-and-forget: a scheduler outage does not back-fill missed chunks
--   automatically. Backfill goes through the immediate-mode dashboard
--   endpoint ('postFinanceManagementReconciliationTrigger').
module SharedLogic.Allocator.Jobs.Reconciliation.ReconciliationScheduler
  ( runReconciliationSchedulerJob,
  )
where

import qualified Data.Map.Strict as Map
import Kernel.Beam.Lib.UtilsTH (HasSchemaName)
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Finance.Reconciliation.Job as ReconJob
import qualified Lib.Finance.Reconciliation.Runner as ReconRunner
import qualified Lib.Finance.Reconciliation.Types as ReconT
import qualified Lib.Finance.Storage.Beam.BeamFlow as FinBeamFlow
import Lib.Scheduler
import Lib.Scheduler.JobStorageType.DB.Table (SchedulerJobT)
import qualified Lib.Scheduler.JobStorageType.SchedulerType as JC
import SharedLogic.Allocator (AllocatorJobType (..))
import SharedLogic.Allocator.Jobs.Reconciliation.Reconciliation (reconciliationRegistry)
import Storage.Beam.SchedulerJob ()

runReconciliationSchedulerJob ::
  forall m r c.
  ( FinBeamFlow.BeamFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m,
    MonadIO m,
    HasShortDurationRetryCfg r c,
    HasField "maxShards" r Int,
    HasField "schedulerSetName" r Text,
    HasField "schedulerType" r SchedulerType,
    HasField "jobInfoMap" r (Map.Map Text Bool),
    HasField "blackListedJobs" r [Text],
    JobCreatorEnv r,
    HasSchemaName SchedulerJobT
  ) =>
  Job 'ReconciliationScheduler ->
  m ExecutionResult
runReconciliationSchedulerJob Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) $ do
  let jobData = jobInfo.jobData
      spec = jobData.spec
      scope = jobData.scope
      mid = Just (Id scope.merchantId)
      moc = Just (Id scope.merchantOperatingCityId)

  case ReconJob.lookupRecipe (reconciliationRegistry :: ReconJob.RecipeRegistry m) spec of
    Nothing -> do
      logError $
        "ReconciliationScheduler: no recipe registered for spec "
          <> ReconT.specKey spec
          <> "; dropping scheduler chain."
      pure Complete
    Just recipe -> do
      now <- getCurrentTime
      let buffer = recipe.settlementBuffer
          plan = recipe.chunkPlan
          duration = ReconT.chunkDuration plan
          -- Round the cutoff down to a chunk boundary so runs align on a
          -- stable grid, and back off one chunk to guarantee the buffer.
          chunkEnd = ReconRunner.latestClosedChunkEnd plan (addUTCTime (negate buffer) now)
          chunkStart = addUTCTime (negate duration) chunkEnd
          range = ReconT.DateRange {from = chunkStart, to = chunkEnd}
          recipeInput = ReconJob.RecipeJobInput {spec = spec, scope = scope, range = range}

      logInfo $
        "ReconciliationScheduler firing: spec="
          <> ReconT.specKey spec
          <> " enqueueing chunk ["
          <> show chunkStart
          <> ", "
          <> show chunkEnd
          <> ")"

      -- 1. Enqueue the recon job for exactly one chunk.
      JC.createJobIn @_ @'Reconciliation mid moc 0 recipeInput

      -- 2. Re-enqueue self one chunk duration from now. A failed job affects
      --    only its own chunk; the scheduler keeps advancing regardless.
      let selfDelay = diffUTCTime (addUTCTime duration now) now
      JC.createJobIn @_ @'ReconciliationScheduler mid moc selfDelay jobData

      pure Complete
