{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Events-window behaviour rulebooks: "y offences in the last X events", for any behaviour
-- whose pack has 'Packs.EventsSupport' (window entity, key, mark mode, Events overlays).
--
-- * A rulebook version's type and X live in its own rows: app_dynamic_logic_element.settings,
--   {"ruleType": "EVENTS", "windowSize": X}, saved with the rules. No settings (every older
--   version) or {"ruleType": "TIME"} = TIME (daily/weekly counters).
-- * How many events each window keeps in Redis (Z) is one per-city config for all behaviours
--   (TransporterConfig.eventsWindowSize, see 'DriverPool.cityEventsWindowSize').
-- * The tracker library does the window write and read; this module only supplies the
--   settings it can't look up itself (the pack's key and mark, the city's Z, the version's X).
-- * Writes: 'recordEventsWindow', called by the behaviour at its own '0' (eligible) and '1'
--   (offence) points, next to its counter writes, which stay as they are.
-- * Reads: 'SharedLogic.BehaviourManagement.RunRules' picks the version once, gets which window
--   and X to count from 'eventsReadFor', and the tracker's snapshot adds counters.events.
module SharedLogic.BehaviourManagement.EventsWindow
  ( validateWindowSettings,
    eventsWindowSizeErrors,
    isEventsVersion,
    versionOverlayKeys,
    getWindowConfigs,
    recordEventsWindow,
    EventsRead (..),
    eventsReadFor,
    reportWindowProblem,
    validateEventsRollout,
    rowsSettings,
    eventsWindowSizeOf,
  )
where

import qualified Data.Aeson as A
import Data.List (nub)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.BehaviorTracker.EventsWindow as BTEW
import qualified Lib.BehaviorTracker.EventsWindowSettings as EWS
import qualified Lib.BehaviorTracker.Recorder as BTRecorder
import qualified Lib.BehaviorTracker.Types as BTT
import Lib.Yudhishthira.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Yudhishthira.Storage.CachedQueries.AppDynamicLogicElement as CADLE
import qualified Lib.Yudhishthira.Tools.TimeBoundRollout as LYTB
import qualified Lib.Yudhishthira.Types as LYT
import qualified Lib.Yudhishthira.Types.AppDynamicLogicElement as DTADLE
import qualified SharedLogic.BehaviourManagement.Packs as Packs
import qualified SharedLogic.DriverPool as DriverPool
import Tools.Metrics (CoreMetrics (..))

-- | What rules read the events window's counts under, e.g. counters.events.actionCount.
eventsFactPrefix :: Text
eventsFactPrefix = "counters." <> BTEW.eventsCounterName

-- | Whether any rule reads the events window, e.g. {"var": "counters.events.actionCount"}.
rulesReadEventsWindow :: [A.Value] -> Bool
rulesReadEventsWindow = any (T.isInfixOf eventsFactPrefix . decodeUtf8 . A.encode)

parseSettings :: A.Value -> Maybe LYT.RulebookSettings
parseSettings value = case A.fromJSON value of
  A.Success meta -> Just meta
  A.Error _ -> Nothing

-- | A version's settings (the same on every row of the version).
rowsSettings :: [DTADLE.AppDynamicLogicElement] -> Maybe LYT.RulebookSettings
rowsSettings rows = listToMaybe rows >>= (.settings) >>= parseSettings

-- | X of an EVENTS version, Nothing for TIME.
eventsWindowSizeOf :: Maybe LYT.RulebookSettings -> Maybe Int
eventsWindowSizeOf = \case
  Just meta | meta.ruleType == LYT.EVENTS -> meta.windowSize
  _ -> Nothing

versionsText :: [Int] -> Text
versionsText vs = T.intercalate ", " (map (\v -> "V" <> show v) vs)

-- | What is wrong with a TransporterConfig patch's events window size (Z), given the city's value
-- before and after the patch. Only a changed value is checked, so an unrelated patch is never
-- blocked by a value that was already wrong.
eventsWindowSizeErrors :: Int -> Int -> [Text]
eventsWindowSizeErrors current patched
  | patched == current = []
  | patched < 1 || patched > EWS.maxWindowSize =
    [ "eventsWindowSize must be between 1 and " <> show EWS.maxWindowSize <> " (the most events each driver's window may keep), got "
        <> show patched
        <> "."
    ]
  | otherwise = []

-- | Checks a save or dry-run request's settings before anything is stored, against the
-- requesting city's window size (Z). The library then saves the settings with the rules, in
-- the same write.
validateWindowSettings :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> LYT.AppDynamicLogicReq -> m ()
validateWindowSettings merchantOpCityId req = do
  let eventsCapable = req.domain `elem` Packs.eventsCapableDomains
      readsEvents = rulesReadEventsWindow req.rules
  meta <- case req.settings of
    Nothing -> pure Nothing
    Just value -> case parseSettings value of
      Just meta -> pure (Just meta)
      Nothing
        | eventsCapable -> throwError $ InvalidRequest "settings must be {\"ruleType\": \"EVENTS\", \"windowSize\": X} or {\"ruleType\": \"TIME\"}."
        | otherwise -> pure Nothing -- other domains may store their own settings here
  case meta of
    Just m | m.ruleType == LYT.EVENTS -> do
      unless eventsCapable $
        throwError $
          InvalidRequest $
            "This behaviour (" <> show req.domain <> ") can't use \"last X events\" rulebooks yet: it doesn't record an events window. Save it as TIME. Behaviours that support it: "
              <> T.intercalate ", " (map show Packs.eventsCapableDomains)
              <> "."
      x <- fromMaybeM (InvalidRequest "A \"last X events\" rulebook needs X (windowSize): how many recent events the rules judge.") m.windowSize
      when (x < 1 || x > EWS.maxWindowSize) $
        throwError $ InvalidRequest $ "X = " <> show x <> " is out of range: X must be between 1 and " <> show EWS.maxWindowSize <> "."
      cityZ <- DriverPool.cityEventsWindowSize merchantOpCityId
      when (x > cityZ) $
        throwError $ InvalidRequest $ xAboveZText x cityZ
      unless readsEvents $
        throwError $
          InvalidRequest $
            "The rulebook is being saved as \"last X events\" (X = " <> show x <> "), but no rule uses the offences in the last X events, so X would have no effect. Save it as TIME, or add a rule on the last X events."
    _ -> do
      when (isJust (meta >>= (.windowSize))) $
        throwError $ InvalidRequest "X (windowSize) is only used by \"last X events\" rulebooks. Remove it, or save the rulebook as EVENTS."
      when (eventsCapable && readsEvents) $
        throwError $
          InvalidRequest $
            "These rules use the offences in the last X events, but the rulebook is being saved as TIME (daily/weekly), where that count doesn't exist, so they would never fire. Save it as EVENTS with an X."

-- | The save/set-live refusal for an X above the city's Z.
xAboveZText :: Int -> Int -> Text
xAboveZText x cityZ =
  "X = " <> show x <> " is more than this city keeps per driver (Z = " <> show cityZ <> "). Lower X or raise the city's events window size."

-- | Whether a version was saved as EVENTS (from its settings). Cached.
isEventsVersion :: BeamFlow m r => LYT.LogicDomain -> Int -> m Bool
isEventsVersion domain version = isJust . eventsWindowSizeOf . rowsSettings <$> CADLE.findByDomainAndVersion domain version

-- | The pack's overlays for a version of this type: its Events overlays for an EVENTS version,
-- its TIME (daily/weekly) overlays otherwise.
versionOverlayKeys :: Packs.BehaviourPack -> Bool -> [Text]
versionOverlayKeys pack isEvents
  | isEvents = maybe [] (.eventsOverlayKeys) pack.eventsSupport
  | otherwise = pack.requiredOverlayKeys

-- | The EVENTS versions of a domain (from their settings) and this city's window size (Z).
getWindowConfigs :: (BeamFlow m r, EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> LYT.LogicDomain -> m LYT.RulebookWindowConfigResp
getWindowConfigs merchantOpCityId domain = do
  rows <- CADLE.findByDomain domain
  cityZ <- DriverPool.cityEventsWindowSize merchantOpCityId
  let firstRowByVersion = Map.fromListWith (\a b -> if a.order <= b.order then a else b) [(row.version, row) | row <- rows]
      versions =
        [ LYT.RulebookWindowConfig {version = v, windowMode = LYT.EVENTS, windowSize = x}
          | (v, row) <- Map.toAscList firstRowByVersion,
            Just x <- [eventsWindowSizeOf (row.settings >>= parseSettings)]
        ]
  pure LYT.RulebookWindowConfigResp {cityWindowSize = cityZ, versions = versions}

-- | Writes one event to the entity's window of a behaviour, and nothing else: ELIGIBLE_COUNT
-- appends '0', ACTION_COUNT marks a '1' (the pack's mark mode), keeping the city's newest Z
-- events. The behaviour calls it at its own points, next to its counter writes (which stay as
-- they are, so the two always move together). Does nothing for a behaviour without events
-- support. Every city records, whether an EVENTS version is live or not. A failure is logged
-- and never breaks the caller; the rules then read the window without this event.
recordEventsWindow ::
  (MonadFlow m, Redis.HedisFlow m r, EsqDBFlow m r, CacheFlow m r) =>
  LYT.LogicDomain ->
  Id DMOC.MerchantOperatingCity ->
  -- | entity id, e.g. the driver id
  Text ->
  -- | ELIGIBLE_COUNT ('0') or ACTION_COUNT ('1')
  BTT.CounterType ->
  m ()
recordEventsWindow domain merchantOpCityId entityId event =
  whenJust (Packs.findEventsSupport domain) $ \support -> do
    let what = show domain <> " window of " <> show support.windowEntity <> " " <> entityId
    result <- withTryCatch "eventsWindow:record" $ do
      keep <- DriverPool.cityEventsWindowSize merchantOpCityId
      BTRecorder.recordEventsWindow support.windowEntity support.windowKeyName support.windowMark keep entityId event
    case result of
      Right Nothing -> pure ()
      Right (Just foundLength) ->
        logError $
          "Events window: the " <> what <> " had " <> show foundLength <> " characters, over the limit of " <> show EWS.oversizedWindowLength
            <> " that the window code never writes past, so it was cleared and restarted at this "
            <> show event
            <> " event"
      Left err ->
        logError $ "Events window: " <> show event <> " wasn't recorded in the " <> what <> " because the Redis window update failed: " <> show err

-- | What the picked rulebook version needs in its snapshot.
data EventsRead
  = -- | TIME version: the snapshot as it is (daily/weekly)
    TimeRules
  | -- | EVENTS version: count this window's newest X events as counters.events
    ReadEvents BTT.EventsWindowRead
  | -- | EVENTS version that can't be judged: don't run the rules; the text names the cause
    EventsUnreadable Text

-- | Which window and X the picked version (its rows, already fetched for the orchestrator)
-- needs: nothing for TIME, the pack's window and the version's X for EVENTS. The tracker's
-- snapshot then reads it; callers write the event first, so the counts include it (write then
-- read, as daily/weekly does). Also logs and counts on the error counter: a TIME version whose
-- rules read counters.events (EVENTS_WINDOW_TIME_VERSION_READS_EVENTS), an EVENTS version of a
-- behaviour without a window (EVENTS_WINDOW_SKIPPED) and an X above this city's Z
-- (EVENTS_WINDOW_X_ABOVE_Z, the rules still run on the Z events stored).
eventsReadFor ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, CoreMetrics m) =>
  LYT.LogicDomain ->
  Id DMOC.MerchantOperatingCity ->
  -- | the picked version
  Maybe Int ->
  -- | its rows
  [DTADLE.AppDynamicLogicElement] ->
  m EventsRead
eventsReadFor domain merchantOpCityId mbVersion rows = do
  let versionText = show domain <> " version " <> maybe "(none)" show mbVersion
  case eventsWindowSizeOf (rowsSettings rows) of
    Nothing -> do
      when (rulesReadEventsWindow (map (.logic) rows)) $
        reportWindowProblem logError "EVENTS_WINDOW_TIME_VERSION_READS_EVENTS" $
          versionText <> " has no events window in its settings, so it runs as TIME, but its rules read " <> eventsFactPrefix
            <> ", which only exists for EVENTS versions, so those rules can never fire. Save it again as EVENTS, or set another version live."
      pure TimeRules
    Just x -> case Packs.findEventsSupport domain of
      Nothing -> do
        let cause = versionText <> " is an EVENTS version, but the behaviour keeps no events window, so its rules were skipped. Set a TIME version live."
        reportWindowProblem logError "EVENTS_WINDOW_SKIPPED" cause
        pure $ EventsUnreadable cause
      Just support -> do
        cityZ <- DriverPool.cityEventsWindowSize merchantOpCityId
        when (x > cityZ) $
          reportWindowProblem logWarning "EVENTS_WINDOW_X_ABOVE_Z" $
            versionText <> " judges the last " <> show x <> " events, but this city keeps only " <> show cityZ
              <> " per entity (TransporterConfig.eventsWindowSize), so it counts at most "
              <> show cityZ
              <> ". Raise TransporterConfig.eventsWindowSize to at least "
              <> show x
              <> ", or set a version with a smaller X."
        pure $ ReadEvents BTT.EventsWindowRead {readEntity = support.windowEntity, readKeyName = support.windowKeyName, readLastX = x}

-- | Logs an events window problem and counts it on the error counter under @counterName@.
reportWindowProblem :: (MonadFlow m, CoreMetrics m) => (Text -> m ()) -> Text -> Text -> m ()
reportWindowProblem logFn counterName cause = do
  logFn $ "Events window: " <> cause
  incrementErrorCounter counterName (toException $ InternalError cause)

-- | City guard for a rollout change of a behaviour with events support. The request is the
-- city's whole new live set (the rollout upsert replaces the active rollouts), so it is
-- checked as a whole: Time and Events versions never live together, at most one Events
-- version, Events only without time windows (the live version must not change by the clock),
-- an Events version's X fits this city's window size (Z), and no TIME version whose rules
-- read the events window. Versions at 0%, and time-bounded rollouts with every version at 0%,
-- don't count.
validateEventsRollout :: (BeamFlow m r, EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> [LYT.LogicRolloutObject] -> m ()
validateEventsRollout merchantOpCityId rolloutReq =
  whenJust (listToMaybe [obj.domain | obj <- rolloutReq, obj.domain `elem` Packs.eventsCapableDomains]) $ \domain -> do
    let live = [(obj.timeBounds, rv.version) | obj <- rolloutReq, obj.domain == domain, rv <- obj.rollout, rv.rolloutPercentage > 0]
        liveVersions = nub (map snd live)
        timeBounded = nub [timeBounds | (timeBounds, _) <- live, timeBounds /= LYTB.unboundedTimeBound]
    versionRows <- forM liveVersions $ \v -> (v,) <$> CADLE.findByDomainAndVersion domain v
    let liveEvents = [(v, x) | (v, rows) <- versionRows, Just x <- [eventsWindowSizeOf (rowsSettings rows)]]
        liveTime = [v | (v, _) <- versionRows, v `notElem` map fst liveEvents]
        brokenTime = [v | (v, rows) <- versionRows, v `elem` liveTime, rulesReadEventsWindow (map (.logic) rows)]
    unless (null brokenTime) $
      throwError $
        InvalidRequest $
          versionsText brokenTime <> " can't be set live: its rules use the offences in the last X events, but it was saved as TIME, so those rules would never fire. Save the rulebook again as EVENTS and set the new version live."
    unless (null liveEvents) $ do
      unless (null liveTime) $
        throwError $
          InvalidRequest $
            "Time and Events versions can't be live together in one city: " <> versionsText (map fst liveEvents) <> " count the last X events, "
              <> versionsText liveTime
              <> " count daily/weekly. Set only one type live."
      when (length liveEvents > 1) $
        throwError $
          InvalidRequest $
            "Only one Events version can be live in a city, but this sets " <> versionsText (map fst liveEvents) <> " live. Each has its own X, so drivers would be judged on different windows at random. Set one of them live."
      unless (null timeBounded) $
        throwError $
          InvalidRequest $
            "Events versions can't be used with time-bounded rollouts (" <> T.intercalate ", " timeBounded <> "): the live version would change by the clock, so the same driver would be judged on a different X depending on the time of day. Remove the time-bounded rollouts (or set them to 0%), or use a Time version."
      cityZ <- DriverPool.cityEventsWindowSize merchantOpCityId
      let aboveZ = [(v, x) | (v, x) <- liveEvents, x > cityZ]
      whenJust (listToMaybe aboveZ) $ \(v, x) ->
        throwError $ InvalidRequest $ versionsText [v] <> " can't be set live in this city. " <> xAboveZText x cityZ
