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
    orchestrateByVersion,
    recordAndOrchestrate,
  )
where

import qualified Data.Aeson as A
import qualified Data.Map.Strict as Map
import Kernel.Prelude
import Kernel.Storage.Clickhouse.Config (ClickhouseFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.BehaviorEngine.EventLog as EventLog
import Lib.BehaviorEngine.RuleEvaluator (RuleFetcher, evaluateLogics, evaluateRules)
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
--   output <- recordAndOrchestrate counterConfig actionEvent entityState callerApp mocId lookupCity domain fetchRules
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
  m (Maybe EventLog.BehaviourEventCity) -> -- city names for the metrics event, looked up in the background
  LYT.LogicDomain ->
  RuleFetcher m ->
  m OrchestratedOutput
recordAndOrchestrate config event entityState callerApp mocId lookupCity domain fetchRules = do
  snapshot <- recordAndSnapshot config event entityState
  orchestrate snapshot callerApp mocId lookupCity domain fetchRules

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
  m (Maybe EventLog.BehaviourEventCity) -> -- city names for the metrics event, looked up in the background
  LYT.LogicDomain ->
  RuleFetcher m ->
  m OrchestratedOutput
orchestrate snapshot callerApp mocId lookupCity domain fetchRules = do
  let snapshotJson = A.toJSON snapshot
  logDebug $ "Orchestrating behavior evaluation for action " <> snapshot.actionType
  (output, mbVersion) <- evaluateRules callerApp mocId domain fetchRules snapshotJson
  EventLog.emitBehaviourEvents lookupCity (EventLog.snapshotEventContext callerApp mocId domain mbVersion snapshot) (EventLog.outputEventDetails output)
  pure output

-- | Evaluate one rulebook version, building the snapshot the way that version needs.
--
-- The version is picked once; the same version's window is then read and its rules
-- fetched. ('orchestrate' fetches rules with a fresh random rollout pick, which could pair
-- one version's rules with another version's window.)
--
--   * Time version: the snapshot is evaluated as it is (same as 'orchestrate').
--   * Events version: @counters.events@ is added next to the daily/weekly counters and
--     cooldowns, which the rules still read for their cooldown guards.
--   * Events version without counts: no rules run, and the cause is logged.
--
-- Usage at integration point (e.g. on driver cancellation):
--   let pickVersion = TDL.selectAppDynamicLogicVersion (cast mocId) domain localTime Nothing
--       fetchVersion v = fst <$> TDL.getAppDynamicLogic (cast mocId) domain localTime (Just v) Nothing
--       readWindow v = -- the version's window config: TimeWindow / EventsWindow counts / EventsUnavailable cause
--   output <- orchestrateByVersion snapshot callerApp mocId lookupCity domain pickVersion fetchVersion readWindow
orchestrateByVersion ::
  ( BeamFlow m r,
    MonadFlow m,
    ClickhouseFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    HasYudhishthiraTablesSchema
  ) =>
  BehaviorSnapshot -> -- daily/weekly counters and cooldowns
  LYDL.CallerApp ->
  Id LYT.MerchantOperatingCity ->
  m (Maybe EventLog.BehaviourEventCity) -> -- city names for the metrics event, looked up in the background
  LYT.LogicDomain ->
  m (Maybe Int) -> -- pick the version (Nothing: none live)
  (Int -> m [A.Value]) -> -- that version's rules
  (Int -> m WindowRead) -> -- how that version reads counters
  m OrchestratedOutput
orchestrateByVersion snapshot callerApp mocId lookupCity domain pickVersion fetchVersion readWindow = do
  logDebug $ "Orchestrating behavior evaluation for action " <> snapshot.actionType
  mbVersion <- pickVersion
  case mbVersion of
    Nothing -> do
      logInfo $ "BehaviorEngine: No rulebook version live for domain " <> show domain <> ". Returning empty output."
      pure defaultOrchestratedOutput
    Just version -> do
      -- Window first: when it can't be read, the rules aren't needed at all.
      windowRead <- readWindow version
      case windowRead of
        EventsUnavailable cause -> do
          logError $ "BehaviorEngine: Skipped rules for domain " <> show domain <> " version " <> show version <> " because its events window could not be read: " <> cause
          pure defaultOrchestratedOutput
        TimeWindow -> evaluateVersion version snapshot
        EventsWindow counts -> evaluateVersion version snapshot{counters = Map.insert eventsCounterName counts snapshot.counters}
  where
    evaluateVersion version snapshot' = do
      logics <- fetchVersion version
      output <- evaluateLogics callerApp mocId domain logics (A.toJSON snapshot')
      EventLog.emitBehaviourEvents lookupCity (EventLog.snapshotEventContext callerApp mocId domain (Just version) snapshot') (EventLog.outputEventDetails output)
      pure output
