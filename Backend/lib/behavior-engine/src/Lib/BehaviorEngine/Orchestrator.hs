{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.BehaviorEngine.Orchestrator
  ( orchestrate,
    recordAndOrchestrate,
  )
where

import qualified Data.Aeson as A
import Kernel.Prelude
import Kernel.Storage.Clickhouse.Config (ClickhouseFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.BehaviorEngine.RuleEvaluator (RuleFetcher, evaluateRules)
import Lib.BehaviorEngine.Types
import Lib.BehaviorTracker.Recorder (recordAndSnapshot)
import Lib.BehaviorTracker.Types (ActionEvent, BehaviorSnapshot, CounterConfig)
import Lib.Yudhishthira.Storage.Beam.BeamFlow (BeamFlow, HasYudhishthiraTablesSchema)
import qualified Lib.Yudhishthira.Tools.DebugLog as LYDL
import qualified Lib.Yudhishthira.Types as LYT

-- | Full pipeline: record action → build snapshot → evaluate rules
--
-- Usage at integration point (e.g. on driver cancellation):
--   let fetchRules = \domain -> TDL.getAppDynamicLogic (cast mocId) domain now Nothing Nothing
--   output <- recordAndOrchestrate counterConfig actionEvent entityState callerApp mocId domain fetchRules
--   CE.executeConsequences driverId output.consequences
--   CM.dispatchCommunications driverId output.communications
recordAndOrchestrate ::
  ( BeamFlow m r,
    MonadFlow m,
    ClickhouseFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    Redis.HedisFlow m r,
    HasYudhishthiraTablesSchema
  ) =>
  CounterConfig ->
  ActionEvent ->
  A.Value -> -- entityState
  LYDL.CallerApp ->
  Id LYT.MerchantOperatingCity ->
  LYT.LogicDomain ->
  Maybe Text -> -- optional entityTransactionId for debug logging
  RuleFetcher m ->
  m OrchestratedOutput
recordAndOrchestrate config event entityState callerApp mocId domain mbEntityTransactionId fetchRules = do
  snapshot <- recordAndSnapshot config event entityState
  orchestrate snapshot callerApp mocId domain mbEntityTransactionId fetchRules

-- | Evaluate rules for an existing snapshot (no recording)
--
-- Use when snapshot is already available (e.g. from a read-only check)
-- or when recording happens separately from evaluation.
orchestrate ::
  ( BeamFlow m r,
    MonadFlow m,
    ClickhouseFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    HasYudhishthiraTablesSchema
  ) =>
  BehaviorSnapshot ->
  LYDL.CallerApp ->
  Id LYT.MerchantOperatingCity ->
  LYT.LogicDomain ->
  Maybe Text -> -- optional entityTransactionId for debug logging
  RuleFetcher m ->
  m OrchestratedOutput
orchestrate snapshot callerApp mocId domain mbEntityTransactionId fetchRules = do
  let snapshotJson = A.toJSON snapshot
  logDebug $ "Orchestrating behavior evaluation for action " <> snapshot.actionType
  evaluateRules callerApp mocId domain mbEntityTransactionId fetchRules snapshotJson
