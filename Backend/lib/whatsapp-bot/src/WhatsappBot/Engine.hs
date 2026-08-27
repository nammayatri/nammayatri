-- | The conversation engine — a structurally faithful port of
-- ny-connectors@77325a7 (2026-07-09). ALL @*.ts:NNN@ citations in this package
-- resolve against THAT revision and against nothing at ny-connectors HEAD.
-- @ny-connectors/connectors/src/flow/engine.ts@ (@FlowEngine.handleMessage@,
-- :46-494, its state handlers, flows, and helpers).
--
-- The engine is pure over a handle-parameterized 'BotEnv' @m@ (Monad @m@ only):
-- every effect — backend calls, outbound sends, session/person/registry reads &
-- writes, and time/delay — is reached through a record-of-functions in the env.
-- It NEVER touches real IO, real time, or real delay. Backend calls return
-- @Either 'BotError'@; the engine reproduces the TS per-call try/catch at each
-- site (L4: the unreachable top-level 401 handler at @engine.ts:483-489@ is NOT
-- ported — the per-call swallow subsumes it).
--
-- Divergences from TS are exactly D1-D4 (silent onboarding; no @AWAITING_OTP@
-- state, no @resend_otp@ intercept, no @authId@; @authenticate@ = in-process
-- find-or-create). @pendingAction@ + @resumeAfterAuth@ semantics are kept so the
-- golden backend-call order (authenticate → getSavedLocations) replays.
-- This module is now the ROUTER: 'handleMessage', the intercept chain, the
-- per-state switch, IDLE, the language chooser, and more/help/support. Nothing
-- else lives here.
--
-- 'scopedSessionId' / 'mkUserKey' are no longer re-exported here: they are
-- flow-agnostic conversation keys and now live in (and are imported from)
-- "WhatsappBot.Env". The ride LIFECYCLE — cancel, status, tracking, SOS,
-- mark-safe, call-driver, and the ride-registry writes — lives in
-- "WhatsappBot.Ride". Booking CREATION — ride-type entry, pickup capture, and
-- the flexi/regular search paths — lives in "WhatsappBot.Flow.Booking", which
-- a second flow sits BESIDE (reusing "WhatsappBot.Ride" without importing the
-- booking flow). The dependency edges run
-- @Engine -> {Ride, Flow.Booking, Env}@, @Flow.Booking -> {Ride, Env}@,
-- @Ride -> Env@, and never back: nothing imports this module.
module WhatsappBot.Engine
  ( handleMessage,
  )
where

import Control.Applicative ((<|>))
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import qualified Data.Text as T
import Kernel.Prelude
-- The flow-agnostic conversation primitives now live in Env (the dependency
-- edge runs Engine -> Env and never back).
import WhatsappBot.Env (BotEnv, btn, ensureAuth, mkUserKey, rawInput, reply, replyButtons, resetContext, save, scopedSessionId)
-- Booking CREATION now lives in Flow.Booking (Engine -> Flow.Booking -> {Ride, Env}).
import WhatsappBot.Flow.Booking (bookTriggers, confirmRegularBooking, flexiOffered, handleConfirmingRegularDrop, handleFlexiEndOtp, handlePickup, handlePickupConfirm, handleRegularDrop, handleRideType, hiddenRideTypeButtons, menuRow, prefetchSavedLocations, promptForBookingEntry, promptForPickup, promptForRegularDrop, regularOffered, sendPickupConfirm, sendRegularFareConfirm)
import WhatsappBot.Handles (StoredPerson (..))
import WhatsappBot.I18n (detectLanguage, getAllLanguages, languageCode, parseLanguage, t)
-- Instances only: LanguageStrings / LanguageInfo are dot-accessed via RDP
-- (getField), so their selectors are never referenced by name, but the HasField
-- instances are needed. (Naming them would trip -Wunused-imports under -Werror.)
import WhatsappBot.I18n.Types ()
import WhatsappBot.Messages (formatDialable)
-- The flow-agnostic ride lifecycle now lives in Ride (Engine -> Ride -> Env).
import WhatsappBot.Ride (cancelTriggers, handleCallDriver, handleCancel, handleCancelConfirm, handleMarkSafeTrigger, handleSosTrigger, handleStatus, handleTracking, statusTriggers)
import WhatsappBot.Types

-- ---------------------------------------------------------------------------
-- Entry point (engine.ts:46-494 + app.ts:34-58)
-- ---------------------------------------------------------------------------

-- | The single entry point (webhook + golden harness both call this). Mirrors
-- @app.ts@ (resolveSession first, so saveContext persists) then
-- @engine.ts:46-494@ (hydrate → intercept chain → state switch). The engine
-- reads/writes context via the SessionStore each turn.
handleMessage :: Monad m => BotEnv m -> InboundEvent -> m ()
handleMessage env ev = do
  let merchant = env.cfg.merchant
      sid = scopedSessionId merchant ev
      uk = mkUserKey merchant ev
  -- Create/refresh the session BEFORE handling (else saveContext no-ops).
  _ <- env.sessions.resolveSession sid
  mctx0 <- env.sessions.getContext sid
  let ctx0 = fromMaybe initialContext mctx0
      input = T.strip (rawInput ev)
  -- Hydrate personId/language from the persistent PersonStore (engine.ts:71-84).
  mstored <-
    if isNothing ctx0.personId || isNothing ctx0.language
      then env.persons.getPerson uk
      else pure Nothing
  let ctx1 = case mstored of
        Just sp -> ctx0 {personId = ctx0.personId <|> Just sp.personId, language = ctx0.language <|> sp.language} :: FlowContext
        Nothing -> ctx0
  -- First-contact language detection (engine.ts:91-98): runs once, only when
  -- unset; romanized/Latin input → Nothing → stays on default (English).
  ctx2 <- case (ctx1.language, detectLanguage input) of
    (Nothing, Just d) -> do
      env.sessions.saveContext sid (ctx1 {language = Just d} :: FlowContext)
      pure (ctx1 {language = Just d} :: FlowContext)
    _ -> pure ctx1
  runEngine env ev ctx2

-- ---------------------------------------------------------------------------
-- Top-level intercept chain (engine.ts:103-430) + state switch (:432-481).
-- Order is load-bearing: each guard mirrors the TS `if (...) { ...; return; }`.
-- ---------------------------------------------------------------------------

runEngine :: Monad m => BotEnv m -> InboundEvent -> FlowContext -> m ()
runEngine env ev ctx = do
  let to = ev.fromPhone
      s = t env.cfg.translations ctx.language
      input = T.strip (rawInput ev)
      lower = T.toLower input
  if
      | Just langCode <- T.stripPrefix "lang:" input, isLangCode langCode -> handleLang env ev ctx langCode -- :103-118 (anchored to lang:<word>)
      | input == "choose_language" || input == "more_languages" -> handleChooseLanguage env ev ctx input -- :120-123
      -- 'handleCancel' takes the post-cancel menu row as a parameter: the row is
      -- booking-flow-specific (Flow.Booking) and Ride must not import a flow.
      | lower `elem` cancelTriggers || "cancel:" `T.isPrefixOf` input -> handleCancel env ev ctx (menuRow env ev) input -- :126-129
      | any (`T.isInfixOf` lower) statusTriggers -> case ctx.personId of -- :131-140
        Nothing -> handleIdle env ev ctx {pendingAction = Just PendingStatus} "book"
        Just _ -> handleStatus env ev ctx
      | input == "main_menu" -> do
        -- :142-149
        resetContext env ev
        row <- menuRow env ev s
        replyButtons env to s.welcome row
      | input == "cancel_confirm" || "cancel_confirm:" `T.isPrefixOf` input -> handleCancelConfirm env ev ctx input -- :153-178
      | input == "abort_cancel" -> handleStatus env ev ctx -- :180-183
      | input == "sos_confirm" && isJust ctx.personId -> do
        -- :186-194
        save env ev ctx {state = ConfirmingSos}
        replyButtons env to s.sosConfirm [btn s.yesTriggerSOS "sos_trigger", btn s.noGoBack "sos_cancel"]
      | input == "sos_trigger" && isJust ctx.personId -> handleSosTrigger env ev ctx -- :197-223
      | input == "mark_safe_confirm" && isJust ctx.personId && isJust ctx.sosId -> do
        -- :226-234
        save env ev ctx {state = ConfirmingMarkSafe}
        replyButtons env to s.markSafeConfirm [btn s.yesMarkSafe "mark_safe_trigger", btn s.noGoBack "mark_safe_cancel"]
      | input == "mark_safe_trigger" && isJust ctx.personId && isJust ctx.sosId -> handleMarkSafeTrigger env ev ctx -- :237-249
      | input == "mark_safe_cancel" -> do
        -- :252-257
        save env ev ctx {state = Tracking}
        handleTracking env ev ctx {state = Tracking}
      | input == "call_112" -> reply env to emergencyMsg -- :260-263
      | input == "sos_cancel" -> do
        -- :266-271
        save env ev ctx {state = Tracking}
        handleTracking env ev ctx {state = Tracking}
      | "ride_type:" `T.isPrefixOf` input -> handleRideType env ev ctx (T.drop 10 input) -- :276-292
      | input == "more_ride_types" -> replyButtons env to s.rideTypePrompt (hiddenRideTypeButtons s env.cfg.merchant.rideTypesOrder)
      | input == "pickup_confirm" && isJust ctx.personId && isJust ctx.origin && ctx.state == ConfirmingPickup ->
        handlePickupConfirm env ev ctx -- :293-305
      | input == "pickup_adjust" -> promptForPickup env ev ctx True -- :306-310
      | input == "regular_book" && isJust ctx.personId && isJust ctx.regularEstimateId && ctx.state == ConfirmingRegularFare ->
        confirmRegularBooking env ev ctx -- :312-315
      | input == "regular_change_drop" && isJust ctx.personId -> promptForRegularDrop env ev ctx -- :316-319
      | "flexi_end_otp:" `T.isPrefixOf` input -> handleFlexiEndOtp env ev ctx (T.drop 14 input) -- :325-359
      | input == "more" -> handleMore env ev ctx -- :365-375
      | input == "help" -> handleHelp env ev ctx -- :376-380
      | input == "support" -> handleSupport env ev ctx -- :381-387
      | input == "call_driver" && isJust ctx.personId -> handleCallDriver env ev ctx -- :389-406
      -- (resend_otp :409-421 dropped, D2)
      | input == "__location_pin__" && ctx.state `elem` [Idle, AwaitingPickup, ConfirmingPickup] ->
        handlePickup env ev ctx -- :426-430
      | otherwise -> stateSwitch env ev ctx input -- :432-481

-- | The per-state dispatch (@engine.ts:432-481@). @AWAITING_OTP@ is gone (D2), so
-- every remaining state is handled — no @default@ branch is reachable.
stateSwitch :: Monad m => BotEnv m -> InboundEvent -> FlowContext -> Text -> m ()
stateSwitch env ev ctx input =
  let to = ev.fromPhone
      s = t env.cfg.translations ctx.language
   in case ctx.state of
        Idle -> handleIdle env ev ctx input
        ChoosingLanguage -> handleChooseLanguage env ev ctx input
        AwaitingPickup -> promptForPickup env ev ctx False
        ConfirmingPickup -> sendPickupConfirm env ev ctx Nothing
        FlexiSearching -> reply env to s.flexiFinding
        AwaitingRegularDrop -> handleRegularDrop env ev ctx input
        ConfirmingRegularDrop -> handleConfirmingRegularDrop env ev ctx input
        ConfirmingRegularFare -> sendRegularFareConfirm env ev ctx
        RegularSearching -> reply env to s.regularSearching
        Tracking -> handleTracking env ev ctx
        ConfirmingSos -> replyButtons env to s.sosConfirm [btn s.yesTriggerSOS "sos_trigger", btn s.noGoBack "sos_cancel"]
        ConfirmingMarkSafe -> replyButtons env to s.markSafeConfirm [btn s.yesMarkSafe "mark_safe_trigger", btn s.noGoBack "mark_safe_cancel"]

-- ---------------------------------------------------------------------------
-- IDLE (engine.ts:498-531)
-- ---------------------------------------------------------------------------

-- | @handleIdle@ (@engine.ts:498-531@). Non-book input → silent auth (welcome-back
-- only, no booking side effects) + intro (once) + welcome menu; a book trigger
-- with no auth → silent onboarding then booking entry.
handleIdle :: Monad m => BotEnv m -> InboundEvent -> FlowContext -> Text -> m ()
handleIdle env ev ctx input = do
  let to = ev.fromPhone
  if not (any (`T.isInfixOf` T.toLower input) bookTriggers)
    then do
      -- Resolve identity even on a bare greeting, so a returning known user
      -- gets "welcome back" without having to type "book" first — but skip
      -- the booking-entry hook (prefetchSavedLocations) since this branch
      -- only ever shows the menu. Already-authenticated sessions (personId
      -- set) skip straight through, same as ensureAuth's own fast path, so
      -- this never re-resolves auth or repeats the welcome-back mid-session.
      mctx <- case ctx.personId of
        Just _ -> pure (Just ctx)
        Nothing -> ensureAuth env ev (\_ c -> pure c) ctx
      case mctx of
        Nothing -> pure () -- ensureAuth already replied (session expired) and saved Idle
        Just ctx' -> do
          let s' = t env.cfg.translations ctx'.language
          sendOnboardingIntroOnce env ev ctx'
          row <- menuRow env ev s'
          replyButtons env to s'.welcome row
    else case ctx.personId of
      Nothing -> do
        let ctx1 = if ctx.pendingAction == Just PendingStatus then ctx else ctx {pendingAction = Just PendingBook}
        save env ev ctx1
        mok <- ensureAuth env ev (prefetchSavedLocations env) ctx1
        case mok of
          Nothing -> pure ()
          Just ctx2 -> do
            let keepStatus = ctx2.pendingAction == Just PendingStatus
                ctx3 = if keepStatus then ctx2 else ctx2 {pendingAction = Nothing}
            unless keepStatus $ save env ev ctx3
            promptForBookingEntry env ev ctx3
      Just _ -> promptForBookingEntry env ev ctx

-- ---------------------------------------------------------------------------
-- Language chooser (engine.ts:103-118, 533-563)
-- ---------------------------------------------------------------------------

-- | @lang:<code>@ intercept (@engine.ts:103-118@): switch language, persist on the
-- person record (if any), and re-show the menu in the new language.
handleLang :: Monad m => BotEnv m -> InboundEvent -> FlowContext -> Text -> m ()
handleLang env ev ctx code = case parseLanguage code of
  Nothing -> pure ()
  Just l -> do
    let to = ev.fromPhone
        uk = mkUserKey env.cfg.merchant ev
        ctx1 = ctx {language = Just l, state = Idle}
    save env ev ctx1
    -- tokenStore.updateLanguage: a no-op until a person record exists.
    case ctx.personId of
      Just pid -> env.persons.setPerson uk StoredPerson {personId = pid, language = Just l}
      Nothing -> pure ()
    let newS = t env.cfg.translations (Just l)
    row <- menuRow env ev newS
    replyButtons env to (newS.languageUpdated newS.nativeLanguageName <> newS.whatToDo) row

-- | @choose_language@ / @more_languages@ (@engine.ts:533-563@).
handleChooseLanguage :: Monad m => BotEnv m -> InboundEvent -> FlowContext -> Text -> m ()
handleChooseLanguage env ev ctx input = do
  let s = t env.cfg.translations ctx.language
      to = ev.fromPhone
  if
      | input == "choose_language" -> do
        save env ev ctx {state = ChoosingLanguage}
        replyButtons
          env
          to
          s.selectLanguage
          [ btn "\127470\127475 \2361\2367\2344\2381\2342\2368" "lang:hi",
            btn "\127470\127475 \3221\3240\3277\3240\3233" "lang:kn",
            btn s.moreLanguages "more_languages"
          ]
      | input == "more_languages" ->
        replyButtons env to s.selectLanguage [btn (li.nativeName <> " (" <> li.name <> ")") ("lang:" <> languageCode li.code) | li <- getAllLanguages env.cfg.translations]
      | otherwise -> pure ()

-- | A @lang:<code>@ command is anchored to word-chars after the prefix; @lang:@
-- followed by anything else falls through the intercept chain (only well-formed
-- codes are the switch command, not a message that merely starts with "lang:").
isLangCode :: Text -> Bool
isLangCode c = not (T.null c) && T.all isWordChar c
  where
    isWordChar ch = isAsciiLower ch || isAsciiUpper ch || isDigit ch || ch == '_'

-- ---------------------------------------------------------------------------
-- More / help / support (engine.ts:365-387)
-- ---------------------------------------------------------------------------

-- | "More options" submenu (@engine.ts:365-375@).
handleMore :: Monad m => BotEnv m -> InboundEvent -> FlowContext -> m ()
handleMore env ev ctx = do
  let s = t env.cfg.translations ctx.language
      to = ev.fromPhone
      merchant = env.cfg.merchant
      items =
        [btn s.rideTypeRegular "ride_type:regular" | flexiOffered merchant && regularOffered merchant]
          <> [btn s.howItWorks "help", btn s.contactSupport "support", btn s.mainMenu "main_menu"]
  replyButtons env to s.moreTitle items

-- | "How it works" (@engine.ts:376-380@): intro video + text, then loop to menu.
handleHelp :: Monad m => BotEnv m -> InboundEvent -> FlowContext -> m ()
handleHelp env ev ctx = do
  sendHowItWorks env ev ctx
  let s = t env.cfg.translations ctx.language
  row <- menuRow env ev s
  replyButtons env (ev.fromPhone) s.moreTitle row

-- | Support contact (@engine.ts:381-387@), then loop to menu.
handleSupport :: Monad m => BotEnv m -> InboundEvent -> FlowContext -> m ()
handleSupport env ev ctx = do
  let s = t env.cfg.translations ctx.language
      to = ev.fromPhone
      raw = fromMaybe "" env.cfg.merchant.flexiSupportPhone
      dial = fromMaybe raw (formatDialable (Just raw))
  reply env to (s.supportMessage dial)
  row <- menuRow env ev s
  replyButtons env to s.moreTitle row

-- ---------------------------------------------------------------------------
-- Intro video (engine.ts:1554-1594)
-- ---------------------------------------------------------------------------

-- | Send the one-time onboarding intro video, exactly once per user
-- (@engine.ts:1570-1582@). Fail-open on the store flag.
sendOnboardingIntroOnce :: Monad m => BotEnv m -> InboundEvent -> FlowContext -> m ()
sendOnboardingIntroOnce env ev ctx = do
  let uk = mkUserKey env.cfg.merchant ev
  seen <- env.persons.getIntroSent uk
  unless seen $ do
    sendIntroVideo env ev ctx
    env.persons.setIntroSent uk

-- | Send the configured intro video, if any (@engine.ts:1557-1565@).
sendIntroVideo :: Monad m => BotEnv m -> InboundEvent -> FlowContext -> m ()
sendIntroVideo env ev ctx = do
  let s = t env.cfg.translations ctx.language
      to = ev.fromPhone
  case env.cfg.merchant.flexiIntroVideoUrl of
    Just url -> void $ env.sender.sendVideo to url (Just s.howItWorksCaption)
    Nothing -> pure ()

-- | "How it works" explainer (@engine.ts:1586-1594@): intro video + text steps.
sendHowItWorks :: Monad m => BotEnv m -> InboundEvent -> FlowContext -> m ()
sendHowItWorks env ev ctx = do
  sendIntroVideo env ev ctx
  reply env (ev.fromPhone) ((t env.cfg.translations ctx.language).howItWorksText)

-- ---------------------------------------------------------------------------
-- Small pure/effect helpers
-- ---------------------------------------------------------------------------

-- | Emergency helpline reply (@engine.ts:261@).
emergencyMsg :: Text
emergencyMsg = "\128222 Emergency helpline: *112*\n\nPlease call 112 directly for immediate assistance."
