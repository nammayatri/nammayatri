-- | The background ride tracker — port of
-- @ny-connectors/connectors/src/tracking/ride-tracker.ts@. One @trackerTick@ =
-- one pass over the registry: for each watched booking, read its status, and
-- push the WhatsApp update for each NEW forward transition
-- (assigned → arrived → started → ended/cancelled). Pure over 'TrackerDeps' @m@
-- (Monad @m@ only); the recurring loop + the @ticking@ overlap guard live in the
-- host (rider-app @App.hs@ / the golden harness advances it a tick at a time).
--
-- Delivery-gated, claim-once: each (booking, stage) is claimed atomically
-- ('claimStage', Redis @SET NX@); a failed send releases the claim so the next
-- tick retries (never lose a final-fare / cancelled message).
module WhatsappBot.Tracker
  ( TrackerDeps (..),
    trackerTick,
  )
where

import Kernel.Prelude
import WhatsappBot.Handles
  ( Clock,
    RegisteredRide (..),
    RideRegistry (..),
    SessionStore (..),
    WaSender (..),
  )
import WhatsappBot.I18n.Types (SupportedLanguage)
import WhatsappBot.Messages
  ( BuiltMessage (..),
    RideStage (..),
    buildArrived,
    buildCancelled,
    buildDriverCard,
    buildEnded,
    buildStarted,
    classifyStage,
    stageKey,
  )
import WhatsappBot.Types

-- | What the tracker needs. The @Clock@ is carried for parity / future use (the
-- host owns the loop cadence); the golden harness supplies all five over IORefs.
data TrackerDeps m = TrackerDeps
  { tdRegistry :: RideRegistry m,
    tdGetBookingDetails :: BotAuth -> Text -> m (Either BotError (Maybe BotBookingDetails)),
    tdSender :: WaSender m,
    tdSessions :: SessionStore m,
    tdClock :: Clock m
  }

-- | Progress rank for the non-terminal notify-once stages
-- (@ride-tracker.ts:23@; @ORDER = [confirmed, assigned, arrived, started]@).
-- Unknown/terminal → -1 so it never counts as a forward move here.
stageRank :: Text -> Int
stageRank = \case
  "confirmed" -> 0
  "assigned" -> 1
  "arrived" -> 2
  "started" -> 3
  _ -> -1

-- | One tracker pass (@ride-tracker.ts:75-90@; the @ticking@ guard is the host's).
trackerTick :: (Monad m) => TrackerDeps m -> m ()
trackerTick deps = do
  rides <- deps.tdRegistry.listRides
  mapM_ (processRide deps) rides

-- | @ride-tracker.ts:92-154@.
processRide :: (Monad m) => TrackerDeps m -> RegisteredRide -> m ()
processRide deps entry = do
  -- Silent-auth port: the rider handle is on the registry entry (no token
  -- lookup / skip-on-missing-token as in the TS token-store version).
  eBooking <- deps.tdGetBookingDetails (BotAuth {personId = entry.personId}) entry.bookingId
  case eBooking of
    Left _ -> pure () -- read failed: skip, retry next tick (ride-tracker.ts:103)
    Right Nothing -> pure () -- null / list-fallback disabled (ride-tracker.ts:104)
    Right (Just booking)
      | booking.bookingId /= entry.bookingId -> pure () -- id mismatch guard
      | otherwise -> dispatch deps entry booking (classifyStage booking)

dispatch :: (Monad m) => TrackerDeps m -> RegisteredRide -> BotBookingDetails -> RideStage -> m ()
dispatch deps entry booking stage = case stage of
  StageCancelled -> terminal deps entry "cancelled" (buildCancelled entry.language)
  StageCompleted -> terminal deps entry "completed" (buildEnded booking entry.language)
  _ -> progressive deps entry booking stage

-- | Terminal stage (@ride-tracker.ts:118-139@): claim-once, send, and only on a
-- delivered (or already-claimed) message reset the rider's session + stop
-- watching. A failed send releases the claim and keeps the entry for retry.
terminal :: (Monad m) => TrackerDeps m -> RegisteredRide -> Text -> BuiltMessage -> m ()
terminal deps entry key msg = do
  won <- deps.tdRegistry.claimStage entry.bookingId key
  proceed <-
    if won
      then do
        delivered <- sendMsg deps entry msg
        unless delivered $ deps.tdRegistry.releaseStage entry.bookingId key
        pure delivered
      else pure True -- already claimed by an earlier tick: finalize idempotently
  when proceed $ do
    resetSession deps entry
    deps.tdRegistry.removeRide entry.bookingId

-- | Progressive stage (@ride-tracker.ts:141-153@): advance only on a forward move
-- AND a delivered message.
progressive :: (Monad m) => TrackerDeps m -> RegisteredRide -> BotBookingDetails -> RideStage -> m ()
progressive deps entry booking stage = do
  let reached = stageKey stage
      curRank = stageRank reached
      lastRank = stageRank (fromMaybe "" entry.lastStage)
  when (curRank > lastRank) $ do
    won <- deps.tdRegistry.claimStage entry.bookingId reached
    delivered <-
      if won
        then do
          d <- sendMsg deps entry (buildFor stage booking entry.language)
          unless d $ deps.tdRegistry.releaseStage entry.bookingId reached
          pure d
        else pure True
    when delivered $ deps.tdRegistry.updateRide entry {lastStage = Just reached}

-- | @ride-tracker.ts:156-163@.
buildFor :: RideStage -> BotBookingDetails -> Maybe SupportedLanguage -> BuiltMessage
buildFor stage booking lang = case stage of
  StageAssigned -> buildDriverCard booking lang
  StageArrived -> buildArrived booking lang
  StageStarted -> buildStarted booking lang
  _ -> buildDriverCard booking lang

-- | Send via buttons when present, else text; returns whether WhatsApp accepted
-- it so the caller can retry (@ride-tracker.ts:166-171@).
sendMsg :: (Monad m) => TrackerDeps m -> RegisteredRide -> BuiltMessage -> m Bool
sendMsg deps entry msg =
  if null msg.bmButtons
    then deps.tdSender.sendText entry.toPhone msg.bmText
    else deps.tdSender.sendButtons entry.toPhone msg.bmText msg.bmButtons

-- | On a terminal ride, reset the rider's session so they can book again — but
-- only if it still points at THIS booking (@ride-tracker.ts:177-198@). Keeps this
-- in sync with the engine's @resetContext@ (clears ride + SOS state).
resetSession :: (Monad m) => TrackerDeps m -> RegisteredRide -> m ()
resetSession deps entry = do
  mctx <- deps.tdSessions.getContext entry.sessionUserId
  whenJust mctx $ \ctx ->
    when (ctx.flexiBookingId == Just entry.bookingId || ctx.activeBookingId == Just entry.bookingId) $
      deps.tdSessions.saveContext
        entry.sessionUserId
        ctx{state = Idle,
            flexiBookingId = Nothing,
            flexiSearchId = Nothing,
            flexiQuoteId = Nothing,
            regularSearchId = Nothing,
            regularEstimateId = Nothing,
            regularFare = Nothing,
            activeBookingId = Nothing,
            rideType = Nothing,
            cancelRequested = Just False,
            sosId = Nothing
           }
