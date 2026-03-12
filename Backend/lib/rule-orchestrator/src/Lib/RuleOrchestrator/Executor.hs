{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.RuleOrchestrator.Executor
  ( executeStep,
    executePlan,
  )
where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as KM
import Data.List (sortBy)
import Data.Ord (comparing)
import Kernel.Prelude
import Kernel.Storage.Clickhouse.Config (ClickhouseFlow)
import Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.RuleOrchestrator.Types
import Lib.Yudhishthira.Storage.Beam.BeamFlow (BeamFlow, HasYudhishthiraTablesSchema)
import qualified Lib.Yudhishthira.Tools.DebugLog as LYDL
import qualified Lib.Yudhishthira.Types as LYT
import qualified Tools.DynamicLogic as TDL

-- | Execute all steps in a plan, respecting priority and dependencies
--
-- 1. Sort steps by priority
-- 2. For each step: verify deps are met → build input → execute → collect result
-- 3. Returns all step results in execution order
executePlan ::
  ( BeamFlow m r,
    MonadFlow m,
    Metrics.CoreMetrics m,
    ClickhouseFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    HasYudhishthiraTablesSchema
  ) =>
  Id LYT.MerchantOperatingCity ->
  LYDL.CallerApp ->
  RuleExecutionPlan ->
  A.Value -> -- snapshot as JSON
  m [StepResult]
executePlan mocId callerApp plan snapshotJson = do
  let sortedSteps = sortBy (comparing (.priority)) plan.steps
  foldlM (executeStepWithContext mocId callerApp snapshotJson) [] sortedSteps

-- | Execute a single step, merging previous results into the input
executeStepWithContext ::
  ( BeamFlow m r,
    MonadFlow m,
    Metrics.CoreMetrics m,
    ClickhouseFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    HasYudhishthiraTablesSchema
  ) =>
  Id LYT.MerchantOperatingCity ->
  LYDL.CallerApp ->
  A.Value -> -- snapshot JSON
  [StepResult] -> -- results accumulated so far
  RuleStep ->
  m [StepResult]
executeStepWithContext mocId callerApp snapshotJson previousResults step = do
  -- Check dependencies are met
  let completedStepNames = map (.stepName) previousResults
  let unmetDeps = filter (`notElem` completedStepNames) step.dependsOn
  if not (null unmetDeps)
    then do
      logError $ "Step " <> step.stepName <> " has unmet dependencies: " <> show unmetDeps <> ". Skipping."
      let emptyResult = defaultStepResult step.stepName step.domain
      return $ previousResults <> [emptyResult {errors = ["Unmet dependencies: " <> show unmetDeps]}]
    else do
      -- Build input: merge snapshot + previous step outputs
      let stepInput = buildStepInput snapshotJson previousResults
      result <- executeStep mocId callerApp step stepInput
      return $ previousResults <> [result]

-- | Execute a single rule step using Yudhishthira
--
-- 1. Fetch rules via getAppDynamicLogic for the step's domain
-- 2. If no rules → return empty result (no-op, graceful)
-- 3. Run rules via runLogicsWithDebugLog
-- 4. Parse response into StepResult
executeStep ::
  ( BeamFlow m r,
    MonadFlow m,
    Metrics.CoreMetrics m,
    ClickhouseFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    HasYudhishthiraTablesSchema
  ) =>
  Id LYT.MerchantOperatingCity ->
  LYDL.CallerApp ->
  RuleStep ->
  A.Value -> -- built input for this step
  m StepResult
executeStep mocId callerApp step input = do
  now <- getCurrentTime
  -- Step 1: Fetch rules for this domain + city
  (allLogics, mbVersion) <- TDL.getAppDynamicLogic (cast mocId) step.domain now Nothing Nothing
  if null allLogics
    then do
      logInfo $ "No rules configured for domain " <> show step.domain <> " in step " <> step.stepName <> ". Returning empty result."
      return $ defaultStepResult step.stepName step.domain
    else do
      -- Step 2: Run rules
      logDebug $ "Executing step " <> step.stepName <> " with domain " <> show step.domain
      resp <-
        withTryCatch ("runLogics:" <> step.stepName) $
          LYDL.runLogicsWithDebugLog callerApp mocId step.domain allLogics input
      case resp of
        Left err -> do
          logError $ "Error in step " <> step.stepName <> ": " <> show err
          return $
            StepResult
              { stepName = step.stepName,
                domain = step.domain,
                output = A.Null,
                errors = [show err],
                version = mbVersion
              }
        Right logicResp -> do
          logDebug $ "Step " <> step.stepName <> " completed. Errors: " <> show logicResp.errors
          return $
            StepResult
              { stepName = step.stepName,
                domain = step.domain,
                output = logicResp.result,
                errors = logicResp.errors,
                version = mbVersion
              }

-- | Build input for a step by merging snapshot JSON with previous step outputs
--
-- The merged input looks like:
-- {
--   ...snapshot fields...,
--   "stepResults": {
--     "threshold_check": { ...step 1 output... },
--     "consequence_calc": { ...step 2 output... }
--   }
-- }
buildStepInput :: A.Value -> [StepResult] -> A.Value
buildStepInput snapshotJson previousResults =
  case snapshotJson of
    A.Object obj ->
      let previousOutputs =
            A.Object $
              KM.fromList $
                map (\r -> (AK.fromText r.stepName, r.output)) previousResults
          merged = KM.insert "stepResults" previousOutputs obj
       in A.Object merged
    _ -> snapshotJson -- if snapshot isn't an object, just pass it through
