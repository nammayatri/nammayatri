{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Events-window behaviour rulebooks: "y offences in the driver's last X events".
--
-- * A rulebook version's type and X live in its own rows: app_dynamic_logic_element.settings,
--   {"ruleType": "EVENTS", "windowSize": X}, saved with the rules. No settings (every older
--   version) or {"ruleType": "TIME"} = TIME (daily/weekly counters).
-- * How many events each driver's window keeps in Redis (Z) is a per-city config
--   (TransporterConfig.eventsWindowSize, see 'DriverPool.cityEventsWindowSize').
-- * At a rule check the snapshot builder (Lib.DriverScore) picks the version once, reads X
--   from its rows, counts the newest X events of the driver's window ('cancellationEventsRead')
--   and adds them as counters.events; the orchestrator just evaluates the snapshot.
module SharedLogic.BehaviourManagement.EventsWindow
  ( eventsWindowDomains,
    eventsCounterName,
    validateWindowSettings,
    eventsWindowSizeErrors,
    isEventsVersion,
    getWindowConfigs,
    EventsRead (..),
    cancellationEventsRead,
    validateEventsRollout,
  )
where

import qualified Data.Aeson as A
import Data.List (nub)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DPerson
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.BehaviorTracker.EventsWindow as BTEW
import qualified Lib.BehaviorTracker.EventsWindowSettings as EWS
import qualified Lib.BehaviorTracker.Types as BTT
import Lib.Yudhishthira.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Yudhishthira.Storage.CachedQueries.AppDynamicLogicElement as CADLE
import qualified Lib.Yudhishthira.Tools.TimeBoundRollout as LYTB
import qualified Lib.Yudhishthira.Types as LYT
import qualified Lib.Yudhishthira.Types.AppDynamicLogicElement as DTADLE
import qualified SharedLogic.DriverPool as DriverPool
import Tools.Metrics (CoreMetrics (..))

-- | Behaviours that record an events window (their CounterConfig has an eventsWindow, see
-- DriverPool.rideCancellationCounterConfig). EVENTS can only be saved for these: any other
-- behaviour has no window recorded, so its rules would never see an offence. Adding a
-- behaviour = set its CounterConfig.eventsWindow, add it here, and read its window at evaluation.
eventsWindowDomains :: [LYT.LogicDomain]
eventsWindowDomains = [LYT.CANCELLATION_RATE_BEHAVIOR]

-- | Snapshot key of the events window: rules read counters.events.actionCount.
eventsCounterName :: Text
eventsCounterName = "events"

eventsFactPrefix :: Text
eventsFactPrefix = "counters." <> eventsCounterName

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
  let behaviourDomain = req.domain `elem` eventsWindowDomains
      readsEvents = rulesReadEventsWindow req.rules
  meta <- case req.settings of
    Nothing -> pure Nothing
    Just value -> case parseSettings value of
      Just meta -> pure (Just meta)
      Nothing
        | behaviourDomain -> throwError $ InvalidRequest "settings must be {\"ruleType\": \"EVENTS\", \"windowSize\": X} or {\"ruleType\": \"TIME\"}."
        | otherwise -> pure Nothing -- other domains may store their own settings here
  case meta of
    Just m | m.ruleType == LYT.EVENTS -> do
      unless behaviourDomain $
        throwError $
          InvalidRequest $
            "ruleType EVENTS isn't supported for " <> show req.domain <> " yet: no events window is recorded for it. Supported: "
              <> T.intercalate ", " (map show eventsWindowDomains)
              <> "."
      x <- fromMaybeM (InvalidRequest "ruleType EVENTS needs a windowSize: the number of recent events (X) the rules judge.") m.windowSize
      when (x < 1 || x > EWS.maxWindowSize) $
        throwError $ InvalidRequest $ "windowSize must be between 1 and " <> show EWS.maxWindowSize <> ", got " <> show x <> "."
      cityZ <- DriverPool.cityEventsWindowSize merchantOpCityId
      when (x > cityZ) $
        throwError $
          InvalidRequest $
            "windowSize " <> show x <> " is more than this city keeps per driver (" <> show cityZ
              <> " events, TransporterConfig.eventsWindowSize), so the rules could count at most "
              <> show cityZ
              <> ". Use a windowSize up to "
              <> show cityZ
              <> ", or raise the city's eventsWindowSize first."
      unless readsEvents $
        throwError $
          InvalidRequest $
            "The version is being saved as EVENTS (X = " <> show x <> "), but its rules never read " <> eventsFactPrefix <> ", so the events window would have no effect. Save as TIME, or make the rules read " <> eventsFactPrefix <> "."
    _ -> do
      when (isJust (meta >>= (.windowSize))) $
        throwError $ InvalidRequest "windowSize is only used when ruleType is EVENTS. Remove windowSize, or set ruleType to EVENTS."
      when (behaviourDomain && readsEvents) $
        throwError $
          InvalidRequest $
            "These rules read " <> eventsFactPrefix <> " (offences in the last X events), but the version is being saved as TIME, where that value doesn't exist, so they would never fire. Save with settings {\"ruleType\": \"EVENTS\", \"windowSize\": X}."

-- | Whether a version was saved as EVENTS (from its settings). Cached.
isEventsVersion :: BeamFlow m r => LYT.LogicDomain -> Int -> m Bool
isEventsVersion domain version = isJust . eventsWindowSizeOf . rowsSettings <$> CADLE.findByDomainAndVersion domain version

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

-- | What the picked cancellation rulebook version needs in its snapshot.
data EventsRead
  = -- | TIME version: the snapshot as it is (daily/weekly)
    TimeRules
  | -- | EVENTS version: add these counts (newest X events) as counters.events
    EventsCounts BTT.CounterValues
  | -- | EVENTS version whose window couldn't be read: don't run the rules; the text names the cause
    EventsUnreadable Text

-- | Reads what the picked version (its rows, already fetched for the orchestrator) needs:
-- nothing for TIME, the driver's newest X events for EVENTS (one GET, counted in Haskell).
-- The counters are written before the rules run, so the counts include this cancel when it
-- was the driver's fault (write then read, as daily/weekly does). Also logs and counts on the
-- error counter: a TIME version whose rules read counters.events
-- (EVENTS_WINDOW_TIME_VERSION_READS_EVENTS), an X above this city's Z (EVENTS_WINDOW_X_ABOVE_Z,
-- the rules still run on the Z events stored), an unreadable window (EVENTS_WINDOW_SKIPPED) and
-- a corrupt one (EVENTS_WINDOW_OVERSIZED_RESET).
cancellationEventsRead ::
  (MonadFlow m, Redis.HedisFlow m r, CoreMetrics m) =>
  Id DPerson.Person ->
  -- | this city's window size (Z)
  Int ->
  -- | the picked version
  Maybe Int ->
  -- | its rows
  [DTADLE.AppDynamicLogicElement] ->
  m EventsRead
cancellationEventsRead driverId cityZ mbVersion rows = do
  let versionText = "version " <> maybe "(none)" show mbVersion
  case eventsWindowSizeOf (rowsSettings rows) of
    Nothing -> do
      when (rulesReadEventsWindow (map (.logic) rows)) $
        report logError "EVENTS_WINDOW_TIME_VERSION_READS_EVENTS" $
          "cancellation " <> versionText <> " is a TIME version (no events window in its settings), but its rules read " <> eventsFactPrefix
            <> ", which only exists for EVENTS versions, so those rules can never fire. Save it again as EVENTS, or set another version live."
      pure TimeRules
    Just x -> do
      when (x > cityZ) $
        report logWarning "EVENTS_WINDOW_X_ABOVE_Z" $
          "cancellation " <> versionText <> " judges the last " <> show x <> " rides, but this city keeps only " <> show cityZ
            <> " per driver (TransporterConfig.eventsWindowSize), so it counts at most "
            <> show cityZ
            <> ". Raise the city's eventsWindowSize to at least "
            <> show x
            <> ", or set a version with a smaller X."
      result <- withTryCatch "eventsWindow:read" $ BTEW.getWindow BTT.DRIVER DriverPool.rideCancellationActionType driverId.getId
      case result of
        Left err -> do
          let cause = "the cancellation window of driver " <> driverId.getId <> " couldn't be read because the Redis read failed: " <> show err
          report logError "EVENTS_WINDOW_SKIPPED" cause
          pure $ EventsUnreadable cause
        Right window -> do
          let (counts, mbOversized) = BTEW.countLast x window
          whenJust mbOversized $ \foundLength ->
            report logError "EVENTS_WINDOW_OVERSIZED_RESET" $
              "the cancellation window of driver " <> driverId.getId <> " had " <> show foundLength <> " characters, over the limit of "
                <> show EWS.oversizedWindowLength
                <> " that the window code never writes past, so it was counted as empty for this rule check (the next ride assignment or at-fault cancel clears it)"
          pure $ EventsCounts counts
  where
    report logFn counterName cause = do
      void $ logFn $ "Events window: " <> cause
      incrementErrorCounter counterName (toException $ InternalError cause)

-- | City guard for a rollout change of a behaviour with an events window. The request is the
-- city's whole new live set (the rollout upsert replaces the active rollouts), so it is
-- checked as a whole: Time and Events versions never live together, at most one Events
-- version, Events only without time windows (the live version must not change by the clock),
-- an Events version's X fits this city's window size (Z), and no TIME version whose rules
-- read the events window.
validateEventsRollout :: (BeamFlow m r, EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> [LYT.LogicRolloutObject] -> m ()
validateEventsRollout merchantOpCityId rolloutReq =
  whenJust (listToMaybe [obj.domain | obj <- rolloutReq, obj.domain `elem` eventsWindowDomains]) $ \domain -> do
    let live = [(obj.timeBounds, rv.version) | obj <- rolloutReq, obj.domain == domain, rv <- obj.rollout, rv.rolloutPercentage > 0]
        liveVersions = nub (map snd live)
        timeBounded = nub [obj.timeBounds | obj <- rolloutReq, obj.domain == domain, obj.timeBounds /= LYTB.unboundedTimeBound, not (null obj.rollout)]
    versionRows <- forM liveVersions $ \v -> (v,) <$> CADLE.findByDomainAndVersion domain v
    let liveEvents = [(v, x) | (v, rows) <- versionRows, Just x <- [eventsWindowSizeOf (rowsSettings rows)]]
        liveTime = [v | (v, _) <- versionRows, v `notElem` map fst liveEvents]
        brokenTime = [v | (v, rows) <- versionRows, v `elem` liveTime, rulesReadEventsWindow (map (.logic) rows)]
    unless (null brokenTime) $
      throwError $
        InvalidRequest $
          versionsText brokenTime <> " can't be set live: the rules read " <> eventsFactPrefix
            <> " (offences in the last X events), but the version's settings don't make it EVENTS, so it runs as TIME and those rules would never fire. Save the rulebook again as EVENTS and set the new version live."
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
            "Events versions can't be used with time-bounded rollouts (" <> T.intercalate ", " timeBounded <> "): the live version would change by the clock, so the same driver would be judged on a different X depending on the time of day. Remove the time-bounded rollouts, or use a Time version."
      cityZ <- DriverPool.cityEventsWindowSize merchantOpCityId
      let aboveZ = [(v, x) | (v, x) <- liveEvents, x > cityZ]
      unless (null aboveZ) $
        throwError $
          InvalidRequest $
            versionsText (map fst aboveZ) <> " can't be set live in this city: it judges the last " <> show (maximum (map snd aboveZ))
              <> " rides, but this city keeps only "
              <> show cityZ
              <> " per driver (TransporterConfig.eventsWindowSize). Raise the city's eventsWindowSize first, or use a version with a smaller X."
