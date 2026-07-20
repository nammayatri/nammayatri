{-
   Copyright 2022-23, Juspay India Pvt Ltd

   This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

   as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

   is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

   or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the

   GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
   -}

-- | Reconciliation job runner (thin app-side glue).
--
--   All comparison logic now lives in @Lib.Finance.Reconciliation@ under
--   @finance-kernel@. This module only:
--
--     * looks up the concrete 'Recipe' for the job's spec in the
--       app-side registry ('reconciliationRegistry' below),
--     * runs one chunk via 'Runner.runNextChunk',
--     * translates the 'Runner.RunOutcome' into an 'ExecutionResult'.
--
--   The scheduler enqueues one job per chunk (see
--   'ReconciliationScheduler'), so this runner never enqueues a follow-up
--   itself — 'Finished' means "return Complete and wait for the next
--   scheduler fire."
module SharedLogic.Allocator.Jobs.Reconciliation.Reconciliation
  ( runReconciliationJob,
    reconciliationRegistry,
  )
where

import qualified Data.Map.Strict as Map
import Kernel.Beam.Lib.UtilsTH (HasSchemaName)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Finance.Reconciliation.Job as ReconJob
import qualified Lib.Finance.Reconciliation.Recipe as ReconRecipe
import qualified Lib.Finance.Reconciliation.Runner as ReconRunner
import qualified Lib.Finance.Reconciliation.Types as ReconTypes
import qualified Lib.Finance.Storage.Beam.BeamFlow as FinBeamFlow
import Lib.Scheduler
import Lib.Scheduler.JobStorageType.DB.Table (SchedulerJobT)
import SharedLogic.Allocator (AllocatorJobType (..))
import qualified SharedLogic.Finance.Reconciliation.Recipes.PostpaidDriverFeeVsPaymentOrder as PostpaidDriverFeeVsPaymentOrder
import qualified SharedLogic.Finance.Reconciliation.Recipes.PrepaidDsrVsLedger as PrepaidDsrVsLedger
import qualified SharedLogic.Finance.Reconciliation.Recipes.PrepaidDsrVsSubscription as PrepaidDsrVsSubscription
import qualified SharedLogic.Finance.Reconciliation.Recipes.PrepaidDssrVsSubscription as PrepaidDssrVsSubscription
import qualified SharedLogic.Finance.Reconciliation.Recipes.PrepaidPgPaymentVsSubscription as PrepaidPgPaymentVsSubscription
import qualified SharedLogic.Finance.Reconciliation.Recipes.PrepaidPgPayoutVsPayoutRequest as PrepaidPgPayoutVsPayoutRequest
import qualified SharedLogic.Finance.Reconciliation.Recipes.PrepaidSubscriptionPurchaseVsTransaction as PrepaidSubscriptionPurchaseVsTransaction
import Storage.Beam.SchedulerJob ()
import qualified Storage.Cac.TransporterConfig as SCTC

-- | App-side dispatch table. Extend this map when a new recipe module lands.
--   Kept as a list-comprehension over a where-bound 'allRecipes' so the
--   list gets exactly one place to edit. The 'forall' pin + no local type
--   signature on 'allRecipes' let GHC infer the (transitively-required)
--   MonadReader / MonadIO / HasShortDurationRetryCfg constraints from
--   the recipe bodies themselves.
reconciliationRegistry ::
  forall m r.
  ( FinBeamFlow.BeamFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m
  ) =>
  ReconJob.RecipeRegistry m
reconciliationRegistry =
  Map.fromList
    [ (r.spec, r) | r <- allRecipes
    ]
  where
    allRecipes =
      [ PrepaidDsrVsLedger.recipe,
        PrepaidDsrVsSubscription.recipe,
        PrepaidDssrVsSubscription.recipe,
        PrepaidPgPaymentVsSubscription.recipe,
        PrepaidPgPayoutVsPayoutRequest.recipe,
        PrepaidSubscriptionPurchaseVsTransaction.recipe,
        PostpaidDriverFeeVsPaymentOrder.recipe
      ] ::
        [ReconRecipe.Recipe m]

runReconciliationJob ::
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
  Job 'Reconciliation ->
  m ExecutionResult
runReconciliationJob Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) $ do
  let input = jobInfo.jobData -- RecipeJobInput
      opCityId = Id input.scope.merchantOperatingCityId

  -- Per-merchant kill switch. Old runner honoured this via
  -- transporterConfig.reconciliationJobsEnabled; keep the same contract so
  -- staging / unmigrated merchants can be paused without deleting jobs.
  mbTc <- SCTC.findByMerchantOpCityId opCityId Nothing
  let enabled = maybe False (fromMaybe False . (.reconciliationJobsEnabled)) mbTc
  if not enabled
    then do
      logInfo $
        "Reconciliation disabled for merchant "
          <> input.scope.merchantId
          <> " / city "
          <> input.scope.merchantOperatingCityId
          <> "; skipping "
          <> ReconTypes.specKey input.spec
      pure Complete
    else case ReconJob.lookupRecipe reconciliationRegistry input.spec of
      Nothing -> do
        logError $
          "No recipe registered for spec " <> ReconTypes.specKey input.spec
            <> "; skipping job."
        pure Complete
      Just recipe -> do
        outcome <- ReconRunner.runNextChunk recipe input
        case outcome of
          ReconRunner.Finished -> pure Complete
          ReconRunner.AwaitingSafeWindow retryAt -> pure $ ReSchedule retryAt
