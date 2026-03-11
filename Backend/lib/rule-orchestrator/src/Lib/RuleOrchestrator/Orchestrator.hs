{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.RuleOrchestrator.Orchestrator
  ( orchestrate,
    orchestrateWithPlan,
  )
where

import qualified Data.Aeson as A
import Kernel.Prelude
import Kernel.Storage.Clickhouse.Config (ClickhouseFlow)
import Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.BehaviorTracker.Types (BehaviorSnapshot)
import Lib.RuleOrchestrator.Executor (executePlan)
import Lib.RuleOrchestrator.OutputMerger (mergeResults)
import Lib.RuleOrchestrator.PlanBuilder (buildPlan)
import Lib.RuleOrchestrator.Types
import qualified Lib.Yudhishthira.Tools.DebugLog as LYDL
import qualified Lib.Yudhishthira.Types as LYT
import Lib.Yudhishthira.Storage.Beam.BeamFlow (BeamFlow, HasYudhishthiraTablesSchema)

-- | Full pipeline: build plan → execute all steps → merge outputs
--
-- This is the main entry point for the rule orchestrator.
-- Usage:
--   snapshot <- BT.recordAndSnapshot config event entityState
--   output   <- RO.orchestrate mocId Driver snapshot
orchestrate ::
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
  BehaviorSnapshot ->
  m OrchestratedOutput
orchestrate mocId callerApp snapshot = do
  let plan = buildPlan snapshot
  orchestrateWithPlan mocId callerApp plan snapshot

-- | Execute with a custom/override plan instead of the default
--
-- Use when an action type needs a non-standard rule evaluation flow.
orchestrateWithPlan ::
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
  BehaviorSnapshot ->
  m OrchestratedOutput
orchestrateWithPlan mocId callerApp plan snapshot = do
  let snapshotJson = A.toJSON snapshot
  logDebug $ "Orchestrating " <> show (length plan.steps) <> " rule steps for action " <> show snapshot.actionType
  results <- executePlan mocId callerApp plan snapshotJson
  let output = mergeResults results
  logDebug $ "Orchestration complete. Consequences: " <> show (length output.consequences) <> ", Communications: " <> show (length output.communications)
  return output
