-- | B2 sweep job handler.
--
--   Fires per (spec, scope) at the recipe's own 'sweepInterval'. Runs one
--   sweep pass over the OPEN entry pool via 'Sweep.runSweep', then
--   re-enqueues itself. Honours the same 'reconciliationJobsEnabled'
--   kill switch as the chunk runner — pausing the recon pauses both
--   chunks and sweeps together.
module SharedLogic.Allocator.Jobs.Reconciliation.ReconciliationSweep
  ( runReconciliationSweepJob,
  )
where

import qualified Data.Map.Strict as Map
import Kernel.Beam.Lib.UtilsTH (HasSchemaName)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Finance.Reconciliation.Job as ReconJob
import qualified Lib.Finance.Reconciliation.Sweep as ReconSweep
import qualified Lib.Finance.Reconciliation.Types as ReconT
import qualified Lib.Finance.Storage.Beam.BeamFlow as FinBeamFlow
import Lib.Scheduler
import Lib.Scheduler.JobStorageType.DB.Table (SchedulerJobT)
import qualified Lib.Scheduler.JobStorageType.SchedulerType as JC
import SharedLogic.Allocator (AllocatorJobType (..))
import SharedLogic.Allocator.Jobs.Reconciliation.Reconciliation (reconciliationRegistry)
import Storage.Beam.SchedulerJob ()
import qualified Storage.Cac.TransporterConfig as SCTC

runReconciliationSweepJob ::
  forall m r c.
  ( FinBeamFlow.BeamFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    Hedis.HedisFlow m r,
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
  Job 'ReconciliationSweep ->
  m ExecutionResult
runReconciliationSweepJob Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) $ do
  let jobData = jobInfo.jobData
      spec = jobData.spec
      scope = jobData.scope
      opCityId = Id scope.merchantOperatingCityId
      mid = Just (Id scope.merchantId)
      moc = Just (Id scope.merchantOperatingCityId)

  mbTc <- SCTC.findByMerchantOpCityId opCityId Nothing
  let enabled = maybe False (fromMaybe False . (.reconciliationJobsEnabled)) mbTc
  if not enabled
    then do
      logInfo $
        "ReconciliationSweep disabled for merchant "
          <> scope.merchantId
          <> " / city "
          <> scope.merchantOperatingCityId
          <> "; dropping "
          <> ReconT.specKey spec
      pure Complete
    else case ReconJob.lookupRecipe reconciliationRegistry spec of
      Nothing -> do
        logError $
          "ReconciliationSweep: no recipe registered for spec "
            <> ReconT.specKey spec
            <> "; dropping sweep chain."
        pure Complete
      Just recipe -> do
        outcome <- ReconSweep.runSweep recipe scope
        logInfo $
          "ReconciliationSweep completed: spec="
            <> ReconT.specKey spec
            <> " loaded="
            <> show outcome.sweepLoaded
            <> " forceClosed="
            <> show outcome.sweepForceClosed
            <> " reclassified="
            <> show outcome.sweepReclassified

        -- Re-enqueue self one sweepInterval from now. Self-driving loop
        -- like the chunk-scheduler: a failed pass doesn't stop the chain
        -- because the job scheduler retries this fire, and success just
        -- enqueues the next fire below.
        JC.createJobIn @_ @'ReconciliationSweep mid moc recipe.sweepInterval jobData
        pure Complete
