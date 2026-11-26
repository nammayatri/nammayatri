{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Runs a behaviour's rulebook for one event, TIME or EVENTS alike, with no behaviour-specific
-- code: any behaviour (driver app or kafka-consumers) can call it once its counters are written.
module SharedLogic.BehaviourManagement.RunRules
  ( runBehaviourRules,
  )
where

import qualified Data.Aeson as A
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Kernel.Prelude
import Kernel.Storage.Clickhouse.Config (ClickhouseFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.BehaviorEngine.Orchestrator as BEOrch
import qualified Lib.BehaviorEngine.Types as BET
import qualified Lib.BehaviorTracker.EventsWindowSettings as EWS
import qualified Lib.BehaviorTracker.Snapshot as BTSnap
import qualified Lib.BehaviorTracker.Types as BTT
import Lib.Yudhishthira.Storage.Beam.BeamFlow (BeamFlow, HasYudhishthiraTablesSchema)
import qualified Lib.Yudhishthira.Tools.DebugLog as LYDL
import qualified Lib.Yudhishthira.Types as LYT
import qualified Lib.Yudhishthira.Types.AppDynamicLogicElement as Fields
import qualified SharedLogic.BehaviourManagement.ConsequenceDispatcher as BehaviorDispatch
import qualified SharedLogic.BehaviourManagement.EventsWindow as EventsWindow
import Tools.DynamicLogic (getAppDynamicLogicElements)
import Tools.Metrics (CoreMetrics)

-- | Picks the city's live version of the behaviour once and fetches its rows once. The tracker
-- builds the snapshot: for an EVENTS version it adds the entity's newest X events (the pack's
-- window) as counters.events; a TIME version gets the snapshot as today. The snapshot carries
-- the cooldown tags the caller passes, and the orchestrator evaluates the same rows (no second
-- fetch). If an EVENTS window can't be read, the rules are skipped (logged,
-- EVENTS_WINDOW_SKIPPED) and nothing is applied; a corrupt one is counted as empty (logged,
-- EVENTS_WINDOW_OVERSIZED_RESET).
-- The caller dispatches the returned consequences and communications, as before.
runBehaviourRules ::
  ( BeamFlow m r,
    HasYudhishthiraTablesSchema,
    MonadFlow m,
    ClickhouseFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    Redis.HedisFlow m r,
    CoreMetrics m
  ) =>
  LYT.LogicDomain ->
  Id DMOC.MerchantOperatingCity ->
  -- | the city's local time (picks the version of a time-bounded rollout)
  UTCTime ->
  -- | counter config of the snapshot (daily/weekly periods)
  BTT.CounterConfig ->
  -- | the event; its entity id is also the window's
  BTT.ActionEvent ->
  -- | entity state for the snapshot
  A.Value ->
  -- | cooldown tags the rules check
  [Text] ->
  m BET.OrchestratedOutput
runBehaviourRules domain merchantOpCityId localTime counterConfig actionEvent entityState cooldownTags = do
  (versionRows, mbVersion) <- getAppDynamicLogicElements (cast merchantOpCityId) domain localTime Nothing Nothing
  eventsRead <- EventsWindow.eventsReadFor domain merchantOpCityId mbVersion versionRows
  let versionText = show domain <> " version " <> maybe "(none)" show mbVersion
      skipRules cause = do
        logError $ "BehaviorEngine: Skipped rules for " <> versionText <> " because its events window could not be read: " <> cause
        pure BET.defaultOrchestratedOutput
  case eventsRead of
    EventsWindow.EventsUnreadable cause -> skipRules cause
    _ -> do
      let mbWindow = case eventsRead of
            EventsWindow.ReadEvents window -> Just window
            _ -> Nothing
      built <- BTSnap.buildSnapshotWithEvents counterConfig actionEvent entityState cooldownTags mbWindow
      case built of
        Left cause -> do
          EventsWindow.reportWindowProblem logError "EVENTS_WINDOW_SKIPPED" $ versionText <> ": " <> cause
          skipRules cause
        Right (snapshot, mbOversized) -> do
          whenJust mbOversized $ \foundLength ->
            EventsWindow.reportWindowProblem logError "EVENTS_WINDOW_OVERSIZED_RESET" $
              versionText <> ": the events window of " <> actionEvent.entityId <> " had " <> show foundLength <> " characters, over the limit of "
                <> show EWS.oversizedWindowLength
                <> " that the window code never writes past, so it was counted as empty for this rule check (the next window write clears it)"
          BEOrch.orchestrate snapshot LYDL.Driver (cast merchantOpCityId) (BehaviorDispatch.behaviourEventCity merchantOpCityId) domain (\_ -> pure (map (.logic) versionRows, mbVersion))
