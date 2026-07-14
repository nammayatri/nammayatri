-- | Flow-agnostic ride lifecycle: everything that operates on a booking rather
-- than on a particular way of creating one.
--
-- This is the substrate any WhatsApp flow reuses — cancel, status, tracking,
-- SOS, mark-safe, call-driver — deliberately separated from the booking flow so
-- that a second flow can reuse it WITHOUT importing the flexi/regular search
-- engine. It knows about bookings and the ride registry; it does NOT know how a
-- booking was created.
--
-- This module must NEVER import @WhatsappBot.Engine@, nor any
-- @WhatsappBot.Flow.*@ module: the dependency edges run
-- @Engine -> {Flow.Booking, Ride} -> Env@ and never back. That is why
-- 'handleCancel' takes the post-cancel MENU ROW as a parameter — the row is
-- flow-specific (the booking flow's lives in @WhatsappBot.Flow.Booking@), and
-- reaching for it directly would make this module depend on a flow.
module WhatsappBot.Ride
  ( -- * Ride resolution — the two policies, named
    currentRide,
    listedBookings,
    BookingWindow (..),

    -- * Lifecycle handlers
    handleCancel,
    handleCancelConfirm,
    handleStatus,
    handleTracking,
    handleSosTrigger,
    handleMarkSafeTrigger,
    handleCallDriver,
    registerRide',
    bumpStage,
    buildTrackingLink,
    isValidBookingId,
    cancelTriggers,
    statusTriggers,
  )
where

import Control.Applicative ((<|>))
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.List (sortOn)
import Data.Ord (Down (..))
import qualified Data.Text as T
import Data.Time (addUTCTime)
import Kernel.Prelude
import WhatsappBot.Env (BotEnv, btn, mkUserKey, reply, replyButtons, replyWithMenu, resetContext, save, scopedSessionId)
import WhatsappBot.Handles (RegisteredRide (..))
import WhatsappBot.I18n (LanguageStrings, t)
-- Instances only: LanguageStrings is dot-accessed via RDP (getField), so its
-- selectors are never referenced by name, but the HasField instances are needed.
-- (Naming them would trip -Wunused-imports under -Werror.)
import WhatsappBot.I18n.Types ()
import WhatsappBot.Messages (RideStage (..), classifyStage)
import WhatsappBot.Types

-- ---------------------------------------------------------------------------
-- Ride resolution: the two policies, named (engine.ts:1599-1612 + the five
-- getActiveBookings call sites)
-- ---------------------------------------------------------------------------

-- | The rider's current ride, resolved LEDGER-FIRST: the 'RideRegistry' is the
-- authoritative index of rides this bot created (an entry is written by
-- 'registerRide'' the moment a booking is confirmed, and stages are claimed
-- atomically via Redis @SET NX@), so it is consulted instead of a backend
-- listing wherever the ride is known to be registered.
--
-- Returns the 'BotAuth' derived from the LEDGER ENTRY rather than from
-- @fromMaybe "" ctx.personId@ — the pattern eight call sites used, two of them
-- ('handleTracking', reached via the @sos_cancel@ / @mark_safe_cancel@ arms)
-- reachable with NO @personId@ guard at all, i.e. with an EMPTY auth handle.
-- The ledger entry records the @personId@ that created the ride and is keyed by
-- the same @mkUserKey@ WhatsApp identity we list by, so it is the same rider,
-- and it is empty only if the ride was itself registered without a personId —
-- exactly the case the old expression already degraded to.
--
-- Terminal (completed/cancelled) → Nothing. Multiple registry entries are
-- sorted newest-first by @createdAt@ (TS parity); typically there is a sole
-- entry.
--
-- There is deliberately NO listing fallback here: adding one would emit an
-- extra @getActiveBookings@ that no golden pins. The listing path is the
-- separate 'listedBookings'. (@engine.ts:1599-1612@; this is the former
-- @resolveActiveBooking@.)
currentRide :: Monad m => BotEnv m -> InboundEvent -> FlowContext -> m (Maybe (BotAuth, BotBookingDetails))
currentRide env ev _ctx = do
  -- The 'FlowContext' is part of the resolution pair's uniform shape (it is what
  -- 'listedBookings' keys its window off, and what the router passes to both);
  -- the ledger-first path needs nothing from it.
  rides <- env.registry.listByUser (mkUserKey env.cfg.merchant ev)
  case listToMaybe (sortOn (Down . (.createdAt)) rides) of
    Nothing -> pure Nothing
    Just r -> do
      let auth = BotAuth r.personId
      eb <- env.backend.getBookingDetails auth r.bookingId
      case eb of
        Right (Just b)
          | classifyStage b `notElem` [StageCompleted, StageCancelled] -> pure (Just (auth, b))
        _ -> pure Nothing

-- | Which @createdAfter@ floor 'listedBookings' passes to @getActiveBookings@.
--
-- The port has THREE distinct policies across the five listing call sites and
-- every one of them is pinned by a golden fixture, so they are NAMED here
-- rather than unified — this type exists precisely so that a future attempt to
-- collapse them has to say so out loud:
--
--   * 'SinceSelectStart' — @sos-no-select-time.json@ pins @getActiveBookings@
--     with a NULL @createdAfter@; giving this path the 24h floor would change
--     that recorded argument.
--   * 'SinceSelectStartOr24h' — @cancel-mid-search.json@ pins @-86400@ (no
--     select time in that session) and @cancel-after-register.json@ pins
--     @-120@ (select time present).
--   * 'SinceExactly' — @regular-happy-path.json@ pins @-120@ from a reference
--     time captured before @selectEstimate@, threaded through the poll loop
--     rather than read back out of the session.
data BookingWindow
  = -- | Raw @ctx.selectStartedAt@, possibly 'Nothing' (= no floor). SOS +
    -- call-driver (@engine.ts:203,393@).
    SinceSelectStart
  | -- | @ctx.selectStartedAt@, else a 24-hour floor. Cancel + cancel-confirm
    -- (@engine.ts:158-161,1290-1293@).
    SinceSelectStartOr24h
  | -- | An explicit reference time captured by the caller before its backend
    -- call. The regular-booking driver poll (@engine.ts:890-906@).
    SinceExactly UTCTime

-- | The rider's active bookings as the BACKEND LISTS them
-- (@getActiveBookings@) — the listing half of the resolution pair, as opposed
-- to 'currentRide', which reads the ride ledger.
--
-- The name describes the SOURCE, not the state of the bookings: entries
-- returned here may or may not also be in the ledger. Do NOT read it as
-- "bookings that are not registered yet", and do NOT reroute a caller onto
-- 'currentRide' on the assumption that a registered ride should use the ledger.
-- Which of the two a site uses is a behavioural contract pinned by fixtures,
-- not a stylistic choice:
--
--   * The two cannot be collapsed. @cancel-mid-search.json@ pins the sequence
--     @[searchFlexi, getFlexiQuotes, getActiveBookings, cancelRide,
--     confirmQuote]@ with NO ride registered — in the window between
--     @confirmQuote@ and 'registerRide'' the ledger is empty, so a ledger-first
--     lookup would return 'Nothing' there and skip the cancel entirely. This is
--     the only path that works then. Two policies, two functions.
--   * The cancel / SOS / call-driver sites call this UNCONDITIONALLY, including
--     long after the ride IS registered. That is preserved behaviour, not an
--     oversight: switching them to 'currentRide' would swap @getActiveBookings@
--     for @getBookingDetails@, which is exactly the regression
--     @cancel-after-register.json@ was written to catch (it records
--     @getActiveBookings [-120]@ through this path, post-registration).
--
-- A @Left@ is flattened to @[]@ (the TS per-call try/catch), matching what all
-- five sites did with their own @Either@.
listedBookings :: Monad m => BotEnv m -> BotAuth -> FlowContext -> BookingWindow -> m [BotBookingDetails]
listedBookings env auth ctx window = do
  createdAfter <- case window of
    SinceSelectStart -> pure ctx.selectStartedAt
    SinceSelectStartOr24h -> case ctx.selectStartedAt of
      Just tt -> pure (Just tt)
      Nothing -> Just . addUTCTime (-86400) <$> env.clock.now
    SinceExactly tt -> pure (Just tt)
  eb <- env.backend.getActiveBookings auth createdAfter
  pure (either (const []) identity eb)

-- ---------------------------------------------------------------------------
-- Status / tracking (engine.ts:1189-1280, 1599-1612)
-- ---------------------------------------------------------------------------

-- | @handleStatus@ (@engine.ts:1236-1280@): the rider's active ride + Call/Cancel.
handleStatus :: Monad m => BotEnv m -> InboundEvent -> FlowContext -> m ()
handleStatus env ev ctx = do
  let s = t ctx.language
      to = ev.fromPhone
  mb <- currentRide env ev ctx
  case mb of
    Nothing -> do
      resetContext env ev
      replyButtons env to s.noActiveRidesBook [btn s.bookARide "book"]
    Just (_auth, b) -> do
      let trackingLink = buildTrackingLink env.cfg.merchant b
          lns =
            [s.activeRide]
              <> maybe [] (\d -> [s.driverLabel d]) b.driverName
              <> maybe [] (\v -> [s.vehicleLabel v]) b.vehicleNumber
              <> maybe [] (\p -> [s.phoneLabel p]) b.driverNumber
              <> maybe [] (\o -> [s.otpLabel o]) b.rideOtp
              <> ["\n" <> s.track <> "\n" <> trackingLink]
          buttons =
            maybe [] (const [btn s.callDriver "call_driver"]) b.driverNumber
              <> [btn s.cancelRide ("cancel_confirm:" <> b.bookingId)]
      replyButtons env to (T.intercalate "\n" lns) buttons

-- | @handleTracking@ (@engine.ts:1189-1234@): ride status + SOS/112/Cancel.
handleTracking :: Monad m => BotEnv m -> InboundEvent -> FlowContext -> m ()
handleTracking env ev ctx = do
  let s = t ctx.language
      to = ev.fromPhone
  mb <- currentRide env ev ctx
  case mb of
    Nothing -> do
      resetContext env ev
      replyButtons env to s.noActiveRidesBook [btn s.bookARide "book"]
    Just (_auth, b) -> do
      let rideStatusUp = maybe "" T.toUpper b.rideStatus
          statusText = if rideStatusUp == "INPROGRESS" then s.rideInProgressStatus else s.rideNotStarted
          trackingLink = buildTrackingLink env.cfg.merchant b
          lns =
            [statusText]
              <> maybe [] (\d -> [s.driverLabel d]) b.driverName
              <> maybe [] (\v -> [s.vehicleLabel v]) b.vehicleNumber
              <> ["\n" <> s.track <> "\n" <> trackingLink]
          sosBtn = case ctx.sosId of
            Just _ -> btn s.markSafeButton "mark_safe_confirm"
            Nothing -> btn s.sosButton "sos_confirm"
          buttons = [sosBtn, btn s.call112Button "call_112", btn s.cancelRide ("cancel_confirm:" <> b.bookingId)]
      replyButtons env to (T.intercalate "\n" lns) buttons

-- | Build a tracking link (@engine.ts:1471-1477@): the merchant's URL template with
-- the ride id substituted (falling back to the bookingId until a ride is assigned,
-- and to the default template if the merchant's URL is unset — TS @|| default@).
-- The link text is not golden-projected.
buildTrackingLink :: MerchantCtx -> BotBookingDetails -> Text
buildTrackingLink merchant b =
  let template = if T.null merchant.nyTrackingUrl then defaultTrackingUrl else merchant.nyTrackingUrl
   in T.replace "{rideId}" (fromMaybe b.bookingId b.rideId) template

defaultTrackingUrl :: Text
defaultTrackingUrl = "https://www.nammayatri.in/u?vp=shareRide&rideId={rideId}"

-- ---------------------------------------------------------------------------
-- Cancel (engine.ts:1282-1355, 153-178)
-- ---------------------------------------------------------------------------

-- | @handleCancel@ (@engine.ts:1282-1355@): flag a cancel, read the active
-- booking, branch on its status (completed/cancelled/in-progress/other), and on a
-- real cancellable booking call @cancelRide@ + stop the tracker.
--
-- @mkRow@ is the menu row every terminal branch loops back to, threaded straight
-- through to 'replyWithMenu'. It is a parameter for the reason given in the
-- module header: the row belongs to a FLOW, and this module must not import one.
handleCancel :: Monad m => BotEnv m -> InboundEvent -> FlowContext -> (LanguageStrings -> m [OutButton]) -> Text -> m ()
handleCancel env ev ctx mkRow input = do
  let s = t ctx.language
      to = ev.fromPhone
      explicitBookingId = if "cancel:" `T.isPrefixOf` input then Just (T.drop 7 input) else Nothing
  case ctx.personId of
    Nothing -> do
      resetContext env ev
      replyWithMenu env ev mkRow s.cancelled
    Just pid -> do
      let auth = BotAuth pid
      save env ev ctx {cancelRequested = Just True}
      -- Listing, NOT the ledger: cancel must also work in the pre-registration
      -- window (cancel-mid-search), and it keeps using the listing after
      -- registration too (cancel-after-register pins getActiveBookings there).
      bookings <- listedBookings env auth ctx SinceSelectStartOr24h
      let booking = case explicitBookingId of
            Just eid -> find (\b -> b.bookingId == eid) bookings
            Nothing -> case ctx.activeBookingId of
              Just aid -> find (\b -> b.bookingId == aid) bookings <|> listToMaybe bookings
              Nothing -> listToMaybe bookings
          bStatus = maybe "" (\b -> T.toUpper b.bookingStatus) booking
          rStatus = maybe "" (\b -> maybe "" T.toUpper b.rideStatus) booking
          trackedId = explicitBookingId <|> ctx.flexiBookingId <|> ctx.activeBookingId <|> (booking >>= (\b -> Just b.bookingId))
      if
          | bStatus == "COMPLETED" || rStatus == "COMPLETED" -> do
            resetContext env ev
            replyWithMenu env ev mkRow s.rideCompleted
          | bStatus == "CANCELLED" || rStatus == "CANCELLED" -> do
            case trackedId of
              Just tid -> do
                _ <- env.registry.claimStage tid "cancelled"
                env.registry.removeRide tid
              Nothing -> pure ()
            resetContext env ev
            replyWithMenu env ev mkRow s.rideAlreadyCancelled
          | rStatus == "INPROGRESS" -> do
            save env ev ctx {cancelRequested = Just False}
            reply env to s.rideInProgress
          | otherwise -> case booking of
            Just b -> do
              ec <- env.backend.cancelRide auth b.bookingId b.bookingStatus
              case ec of
                Left err -> do
                  resetContext env ev
                  replyWithMenu env ev mkRow (s.cancelFailed err.botErrorMessage)
                Right () -> do
                  let stopId = fromMaybe b.bookingId trackedId
                  _ <- env.registry.claimStage stopId "cancelled"
                  env.registry.removeRide stopId
                  resetContext env ev
                  replyWithMenu env ev mkRow s.rideCancelled
            Nothing -> do
              resetContext env ev
              replyWithMenu env ev mkRow s.cancelled

-- | Cancel confirmation prompt (@engine.ts:153-178@): personalise with the driver
-- name when known.
handleCancelConfirm :: Monad m => BotEnv m -> InboundEvent -> FlowContext -> Text -> m ()
handleCancelConfirm env ev ctx input = do
  let s = t ctx.language
      to = ev.fromPhone
      bookingId = if ":" `T.isInfixOf` input then T.intercalate ":" (drop 1 (T.splitOn ":" input)) else ""
      yesData = if T.null bookingId then "cancel" else "cancel:" <> bookingId
  confirmPrompt <-
    if not (T.null bookingId) && isJust ctx.personId
      then do
        -- Guarded by the enclosing @isJust ctx.personId@, so this auth is never
        -- the empty handle.
        let auth = BotAuth (fromMaybe "" ctx.personId)
        -- Same window as 'handleCancel' — the prompt must describe the very
        -- booking that Yes will cancel.
        bookings <- listedBookings env auth ctx SinceSelectStartOr24h
        -- A failed listing flattens to @[]@ here, which yields @Nothing@ and so
        -- the plain prompt — identical to the old explicit @Left _@ arm.
        let booking = find (\b -> b.bookingId == bookingId) bookings <|> listToMaybe bookings
        case booking >>= (\b -> b.driverName) of
          Just dn -> pure (s.cancelConfirmWithDriver dn (booking >>= (\b -> b.vehicleNumber)))
          Nothing -> pure s.cancelConfirm
      else pure s.cancelConfirm
  replyButtons env to confirmPrompt [btn s.yesCancelIt yesData, btn s.noKeepIt "abort_cancel"]

-- ---------------------------------------------------------------------------
-- SOS / mark-safe (engine.ts:197-249)
-- ---------------------------------------------------------------------------

-- | @sos_trigger@ (@engine.ts:197-223@): find the active ride, trigger SOS.
handleSosTrigger :: Monad m => BotEnv m -> InboundEvent -> FlowContext -> m ()
handleSosTrigger env ev ctx = do
  let s = t ctx.language
      to = ev.fromPhone
      auth = BotAuth (fromMaybe "" ctx.personId)
  -- RAW ctx.selectStartedAt — no 24h floor. sos-no-select-time.json pins
  -- getActiveBookings with a NULL createdAfter on this path.
  bookings <- listedBookings env auth ctx SinceSelectStart
  case bookings of
    (b : _) -> case b.rideId of
      -- SOS targets the RIDE id (rideList[0].id); no assigned ride => nothing to SOS.
      Nothing -> do
        reply env to (s.sosFailed "No active ride found")
        save env ev ctx {state = Tracking}
      Just rid -> do
        esos <- env.backend.triggerSOS auth rid Nothing
        case esos of
          Left err -> do
            reply env to (s.sosFailed err.botErrorMessage)
            save env ev ctx {state = Tracking}
          Right sosId -> do
            save env ev ctx {sosId = Just sosId, state = Tracking}
            replyButtons env to s.sosTriggered [btn s.markSafeButton "mark_safe_confirm"]
    [] -> do
      reply env to (s.sosFailed "No active ride found")
      save env ev ctx {state = Tracking}

-- | @mark_safe_trigger@ (@engine.ts:237-249@): call markRideAsSafe, clear the SOS.
handleMarkSafeTrigger :: Monad m => BotEnv m -> InboundEvent -> FlowContext -> m ()
handleMarkSafeTrigger env ev ctx = do
  let s = t ctx.language
      to = ev.fromPhone
      auth = BotAuth (fromMaybe "" ctx.personId)
  case ctx.sosId of
    Nothing -> pure ()
    Just sosId -> do
      em <- env.backend.markRideAsSafe auth sosId
      case em of
        Left err -> do
          reply env to (s.markSafeFailed err.botErrorMessage)
          save env ev ctx {state = Tracking}
        Right () -> do
          reply env to s.markedSafe
          save env ev ctx {sosId = Nothing, state = Tracking}

-- ---------------------------------------------------------------------------
-- Call driver (engine.ts:389-406)
-- ---------------------------------------------------------------------------

-- | @call_driver@ (@engine.ts:389-406@): reveal the driver's dial number.
handleCallDriver :: Monad m => BotEnv m -> InboundEvent -> FlowContext -> m ()
handleCallDriver env ev ctx = do
  let s = t ctx.language
      to = ev.fromPhone
      auth = BotAuth (fromMaybe "" ctx.personId)
  -- RAW ctx.selectStartedAt, same as the SOS path — no 24h floor.
  bookings <- listedBookings env auth ctx SinceSelectStart
  case bookings of
    (b : _) -> case b.driverNumber of
      Just phone -> reply env to (s.driverPhone phone)
      Nothing -> reply env to s.driverDetailsNotAvailable
    [] -> reply env to s.noActiveRide

-- ---------------------------------------------------------------------------
-- Ride registry
-- ---------------------------------------------------------------------------

-- | Register a booking with the durable ride tracker (@engine.ts:1498-1515@).
registerRide' :: Monad m => BotEnv m -> InboundEvent -> Text -> FlowContext -> Text -> m ()
registerRide' env ev bookingId ctx stage = do
  now <- env.clock.now
  env.registry.registerRide
    RegisteredRide
      { bookingId = bookingId,
        userKey = mkUserKey env.cfg.merchant ev,
        sessionUserId = scopedSessionId env.cfg.merchant ev,
        toPhone = ev.fromPhone,
        merchantLabel = env.cfg.merchant.merchantLabel,
        personId = fromMaybe "" ctx.personId,
        rideType = fromMaybe Flexi ctx.rideType,
        language = ctx.language,
        lastStage = Just stage,
        createdAt = now
      }

-- | Advance the persisted last-pushed stage on a registry entry (@ride.update@).
bumpStage :: Monad m => BotEnv m -> Text -> Text -> m ()
bumpStage env bid stage = do
  mr <- env.registry.getRide bid
  case mr of
    Just r -> env.registry.updateRide r {lastStage = Just stage}
    Nothing -> pure ()

-- ---------------------------------------------------------------------------
-- Small pure helpers / trigger words
-- ---------------------------------------------------------------------------

-- | A server-generated booking id: @^[A-Za-z0-9_-]+$@ (@engine.ts:330@) — rejects
-- path-traversal chars a user could TYPE into the @flexi_end_otp:@ prefix.
isValidBookingId :: Text -> Bool
isValidBookingId b = not (T.null b) && T.all ok b
  where
    ok c = isAsciiUpper c || isAsciiLower c || isDigit c || c == '-' || c == '_'

cancelTriggers :: [Text]
cancelTriggers = ["cancel", "stop", "exit", "quit", "reset"]

statusTriggers :: [Text]
statusTriggers = ["status", "track", "where is my ride"]
