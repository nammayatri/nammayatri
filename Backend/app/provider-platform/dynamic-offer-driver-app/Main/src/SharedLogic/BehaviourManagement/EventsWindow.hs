{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Window settings of behaviour rulebook versions: TIME (daily/weekly counters, the default)
-- or EVENTS ("offences in the driver's last X events"). The mode and X are chosen when a
-- version is saved and stored in app_dynamic_logic_window_config; a version without a row is TIME.
module SharedLogic.BehaviourManagement.EventsWindow
  ( eventsWindowDomains,
    validateWindowSettings,
    saveWindowConfig,
    getWindowConfigs,
    isEventsVersion,
    cancellationWindowRead,
    checkTimeVersionRules,
    validateEventsRollout,
  )
where

import qualified Data.Aeson as A
import Data.List (nub, sortOn)
import qualified Data.Text as T
import qualified Domain.Types.AppDynamicLogicWindowConfig as DWC
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DPerson
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.BehaviorEngine.Types as BET
import qualified Lib.BehaviorTracker.EventsWindow as BTEW
import qualified Lib.BehaviorTracker.EventsWindowSettings as EWS
import qualified Lib.BehaviorTracker.Types as BTT
import Lib.Yudhishthira.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Yudhishthira.Storage.CachedQueries.AppDynamicLogicElement as CADLE
import qualified Lib.Yudhishthira.Tools.TimeBoundRollout as LYTB
import qualified Lib.Yudhishthira.Types as LYT
import qualified SharedLogic.DriverPool as DriverPool
import qualified Storage.CachedQueries.AppDynamicLogicWindowConfig as CQWC
import qualified Storage.Queries.AppDynamicLogicWindowConfig as QWC
import Tools.Metrics (CoreMetrics (..))

-- | Behaviours that record an events window (their CounterConfig has an eventsWindow, see
-- DriverPool.rideCancellationCounterConfig). EVENTS can only be saved for these: any other
-- behaviour has no window recorded, so its rules would never see an offence. Adding a
-- behaviour = set its CounterConfig.eventsWindow, add it here, and read its window at evaluation.
eventsWindowDomains :: [LYT.LogicDomain]
eventsWindowDomains = [LYT.CANCELLATION_RATE_BEHAVIOR]

-- | Checks the window settings of a save or dry-run request before anything is stored.
-- Returns the window size (X) for EVENTS, Nothing for TIME (nothing to store).
validateWindowSettings :: MonadFlow m => LYT.AppDynamicLogicReq -> m (Maybe Int)
validateWindowSettings req = do
  mbSize <- validateModeAndSize req
  -- The type and the rules must agree: TIME has no counters.events in its snapshot, so
  -- rules that read it would silently never fire; EVENTS rules that ignore it judge nothing.
  case (mbSize, rulesReadEventsWindow req.rules) of
    (Nothing, True) ->
      throwError $
        InvalidRequest $
          "These rules read " <> eventsFactPrefix <> " (offences in the last X events), but the version is being saved as TIME, where that value doesn't exist, so they would never fire. Save with windowMode EVENTS and a windowSize."
    (Just size, False) ->
      throwError $
        InvalidRequest $
          "The version is being saved as EVENTS (X = " <> show size <> "), but its rules never read " <> eventsFactPrefix <> ", so the events window would have no effect. Save as TIME, or make the rules read " <> eventsFactPrefix <> "."
    _ -> pure mbSize

-- | Snapshot path of the events window (CounterValues under counters["events"]).
eventsFactPrefix :: Text
eventsFactPrefix = "counters." <> BET.eventsCounterName

-- | Whether any rule reads the events window, e.g. {"var": "counters.events.actionCount"}.
rulesReadEventsWindow :: [A.Value] -> Bool
rulesReadEventsWindow = any (T.isInfixOf eventsFactPrefix . decodeUtf8 . A.encode)

validateModeAndSize :: MonadFlow m => LYT.AppDynamicLogicReq -> m (Maybe Int)
validateModeAndSize req = case (fromMaybe LYT.TIME req.windowMode, req.windowSize) of
  (LYT.TIME, Nothing) -> pure Nothing
  (LYT.TIME, Just _) ->
    throwError $ InvalidRequest "windowSize is only used when windowMode is EVENTS. Remove windowSize, or set windowMode to EVENTS."
  (LYT.EVENTS, mbSize)
    | req.domain `notElem` eventsWindowDomains ->
      throwError $
        InvalidRequest $
          "windowMode EVENTS isn't supported for " <> show req.domain <> " yet: no events window is recorded for it. Supported: "
            <> T.intercalate ", " (map show eventsWindowDomains)
            <> "."
    | otherwise -> case mbSize of
      Nothing -> throwError $ InvalidRequest "windowMode EVENTS needs a windowSize: the number of recent events (X) the rules judge."
      Just size -> do
        when (size < 1 || size > EWS.maxWindowSize) $
          throwError $ InvalidRequest $ "windowSize must be between 1 and " <> show EWS.maxWindowSize <> ", got " <> show size <> "."
        pure (Just size)

-- | Stores an EVENTS version's window size, right after the version itself was saved.
-- The version is already stored at this point, so a failure can't undo it: it comes back as
-- a message for the save response's errors (a 5xx would hide the text), and the version
-- stays TIME until saved again. Nothing = stored.
saveWindowConfig :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id DM.Merchant -> LYT.LogicDomain -> Maybe Int -> Int -> m (Maybe Text)
saveWindowConfig merchantId domain mbVersion windowSize = case mbVersion of
  Nothing -> do
    let msg = "The rulebook for " <> show domain <> " was saved, but no version number came back, so its events window (X = " <> show windowSize <> ") wasn't stored. Don't set it live; save the rulebook again."
    logError $ "Events window: " <> msg
    pure (Just msg)
  Just version -> do
    now <- getCurrentTime
    result <-
      withTryCatch "saveWindowConfig" $
        CQWC.create
          DWC.AppDynamicLogicWindowConfig
            { domain = domain,
              version = version,
              windowMode = LYT.EVENTS,
              windowSize = windowSize,
              merchantId = Just merchantId,
              merchantOperatingCityId = Nothing, -- versions are global
              createdAt = now,
              updatedAt = now
            }
    case result of
      Right () -> do
        logInfo $ "Behaviour rulebook " <> show domain <> " version " <> show version <> " saved as EVENTS, X = " <> show windowSize
        pure Nothing
      Left err -> do
        logError $ "Storing the events window of " <> show domain <> " version " <> show version <> " failed: " <> show err
        pure . Just $
          "Version " <> show version <> " was saved, but storing its events window (X = " <> show windowSize
            <> ") failed in the database, so it would run as TIME. Don't set version "
            <> show version
            <> " live; save the rulebook again."

-- | Window settings of every EVENTS version of a domain, read from the database (never stale).
getWindowConfigs :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => LYT.LogicDomain -> m [LYT.RulebookWindowConfig]
getWindowConfigs domain = do
  rows <- QWC.findAllByDomain domain
  pure $ sortOn (.version) $ map (\row -> LYT.RulebookWindowConfig {version = row.version, windowMode = row.windowMode, windowSize = row.windowSize}) rows

-- | Whether a version was saved as EVENTS (it has a window row; no row = TIME). Cached.
isEventsVersion :: (CacheFlow m r, EsqDBFlow m r) => LYT.LogicDomain -> Int -> m Bool
isEventsVersion domain version = (== Just LYT.EVENTS) . fmap (.windowMode) <$> CQWC.findByDomainAndVersion domain version

-- * The cancellation window at runtime

-- The window itself is written by the generic counters: DriverPool.rideCancellationCounterConfig
-- has an events window ('BTT.MarkLatestEligible'), so every ride assigned (ELIGIBLE) appends
-- '0' and every at-fault cancel (ACTION) marks a '1', in every city, keeping the last
-- 'EWS.maxWindowSize' events. Only the read, at evaluation time, depends on the version.

cancellationDomain :: LYT.LogicDomain
cancellationDomain = LYT.CANCELLATION_RATE_BEHAVIOR

-- | How the picked cancellation rulebook version reads counters (passed to
-- 'Lib.BehaviorEngine.Orchestrator.orchestrateByVersion'): TIME versions read daily/weekly;
-- EVENTS versions also get the counts of the driver's last X events, with that version's own
-- X. The counters are written before the rules run, so the counts include this cancel when
-- it was the driver's fault (write then read, as daily/weekly does).
-- A skip (EventsUnavailable) is also counted on the error counter as EVENTS_WINDOW_SKIPPED,
-- so it shows up next to the app's other errors (the orchestrator logs the cause).
cancellationWindowRead :: (BeamFlow m r, CoreMetrics m) => Id DPerson.Person -> Int -> m BET.WindowRead
cancellationWindowRead driverId version = do
  windowRead <- readForVersion driverId version
  case windowRead of
    BET.EventsUnavailable cause -> incrementErrorCounter "EVENTS_WINDOW_SKIPPED" (toException $ InternalError cause)
    _ -> pure ()
  pure windowRead

readForVersion :: (BeamFlow m r, CoreMetrics m) => Id DPerson.Person -> Int -> m BET.WindowRead
readForVersion driverId version = do
  mbConfig <- CQWC.findByDomainAndVersion cancellationDomain version
  case mbConfig of
    Just config | config.windowMode == LYT.EVENTS -> do
      result <- withTryCatch "eventsWindow:cancellationWindowRead" $ BTEW.readWindowCounts BTT.DRIVER DriverPool.rideCancellationActionType driverId.getId config.windowSize
      case result of
        Right (counts, mbOversized) -> do
          whenJust mbOversized $ reportOversizedWindow driverId
          pure $ BET.EventsWindow counts
        Left err -> pure $ BET.EventsUnavailable $ "the window couldn't be read because the Redis read failed: " <> show err
    _ -> pure BET.TimeWindow

-- | A driver's window was longer than 'EWS.oversizedWindowLength', which the window scripts
-- never write (they always trim), so the key was corrupt. The read counted it as empty instead
-- of working through it, and the driver's next ride assignment or at-fault cancel clears it
-- (the recorder logs that). Logged and counted on the error counter as
-- EVENTS_WINDOW_OVERSIZED_RESET; the rules still run, on the empty window.
reportOversizedWindow :: (MonadFlow m, CoreMetrics m) => Id DPerson.Person -> Int -> m ()
reportOversizedWindow driverId foundLength = do
  let cause =
        "the cancellation window of driver " <> driverId.getId <> " had " <> show foundLength <> " characters, over the limit of "
          <> show EWS.oversizedWindowLength
          <> " that the window code never writes past, so it was counted as empty for this rule check (the next ride assignment or at-fault cancel clears it)"
  logError $ "Events window: " <> cause
  incrementErrorCounter "EVENTS_WINDOW_OVERSIZED_RESET" (toException $ InternalError cause)

-- | Backstop for the save check: a TIME version whose rules read counters.events would
-- silently never fire (its snapshot has no events window). Logged and counted on the error
-- counter as EVENTS_WINDOW_TIME_VERSION_READS_EVENTS; the rules still run as saved.
checkTimeVersionRules :: (BeamFlow m r, CoreMetrics m) => LYT.LogicDomain -> Int -> [A.Value] -> m ()
checkTimeVersionRules domain version logics =
  when (rulesReadEventsWindow logics) $ do
    mbConfig <- CQWC.findByDomainAndVersion domain version
    unless (fmap (.windowMode) mbConfig == Just LYT.EVENTS) $ do
      let cause = show domain <> " version " <> show version <> " is a TIME version, but its rules read " <> eventsFactPrefix <> ", which only exists for EVENTS versions, so those rules can never fire. Save it again as EVENTS, or set another version live."
      logError $ "Events window: " <> cause
      incrementErrorCounter "EVENTS_WINDOW_TIME_VERSION_READS_EVENTS" (toException $ InternalError cause)

-- | City guard for a rollout change of a behaviour with an events window. The request is the
-- city's whole new live set (the rollout upsert replaces the active rollouts), so it is
-- checked as a whole: Time and Events versions never live together, at most one Events
-- version, and Events only without time windows (the live version must not change by the clock).
validateEventsRollout :: BeamFlow m r => [LYT.LogicRolloutObject] -> m ()
validateEventsRollout rolloutReq =
  whenJust (listToMaybe [obj.domain | obj <- rolloutReq, obj.domain `elem` eventsWindowDomains]) $ \domain -> do
    configs <- CQWC.findAllByDomain domain
    let eventsVersions = [config.version | config <- configs, config.windowMode == LYT.EVENTS]
        live = [(obj.timeBounds, rv.version) | obj <- rolloutReq, obj.domain == domain, rv <- obj.rollout, rv.rolloutPercentage > 0]
        liveVersions = nub (map snd live)
        liveEvents = filter (`elem` eventsVersions) liveVersions
        liveTime = filter (`notElem` eventsVersions) liveVersions
        timeBounded = nub [obj.timeBounds | obj <- rolloutReq, obj.domain == domain, obj.timeBounds /= LYTB.unboundedTimeBound, not (null obj.rollout)]
        versionsText vs = T.intercalate ", " (map (\v -> "V" <> show v) vs)
    -- A TIME version whose rules read the events window (e.g. its window row failed to save)
    -- would never fire; don't let it go live.
    brokenTime <- filterM (\v -> rulesReadEventsWindow . map (.logic) <$> CADLE.findByDomainAndVersion domain v) liveTime
    unless (null brokenTime) $
      throwError $
        InvalidRequest $
          versionsText brokenTime <> " can't be set live: the rules read " <> eventsFactPrefix
            <> " (offences in the last X events), but the version has no events window stored, so it runs as TIME and those rules would never fire. Save the rulebook again as EVENTS and set the new version live."
    unless (null liveEvents) $ do
      unless (null liveTime) $
        throwError $
          InvalidRequest $
            "Time and Events versions can't be live together in one city: " <> versionsText liveEvents <> " count the last X events, "
              <> versionsText liveTime
              <> " count daily/weekly. Set only one type live."
      when (length liveEvents > 1) $
        throwError $
          InvalidRequest $
            "Only one Events version can be live in a city, but this sets " <> versionsText liveEvents <> " live. Each has its own X, so drivers would be judged on different windows at random. Set one of them live."
      unless (null timeBounded) $
        throwError $
          InvalidRequest $
            "Events versions can't be used with time-bounded rollouts (" <> T.intercalate ", " timeBounded <> "): the live version would change by the clock, so the same driver would be judged on a different X depending on the time of day. Remove the time-bounded rollouts, or use a Time version."
