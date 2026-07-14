-- | The conversation engine — a structurally faithful port of
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
module WhatsappBot.Engine
  ( handleMessage,
    scopedSessionId,
    mkUserKey,
  )
where

import Control.Applicative ((<|>))
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import qualified Data.Text as T
import Data.Time (addUTCTime)
import Kernel.Prelude
import WhatsappBot.Cities (isWithinServiceArea)
import WhatsappBot.Env (BotEnv)
import WhatsappBot.Handles (RegisteredRide (..), StoredPerson (..))
import WhatsappBot.I18n (LanguageStrings, SupportedLanguage, detectLanguage, getAllLanguages, languageCode, parseLanguage, t)
-- Instances only: LanguageStrings / LanguageInfo are dot-accessed via RDP
-- (getField), so their selectors are never referenced by name, but the HasField
-- instances are needed. (Naming them would trip -Wunused-imports under -Werror.)
import WhatsappBot.I18n.Types ()
import WhatsappBot.Messages (RideStage (..), buildDriverCard, classifyStage, formatDialable)
import WhatsappBot.Types
import WhatsappBot.Util (fmtInt, fmtNum)

-- ---------------------------------------------------------------------------
-- Entry point (engine.ts:46-494 + app.ts:34-58)
-- ---------------------------------------------------------------------------

-- | Merchant-scoped conversation session id — the SessionStore key
-- (@app.ts:43-46@: @session:whatsapp:<merchantId>:<sender>@; here the store
-- adapter owns the @session:@ prefix). Exposed for the webhook layer.
scopedSessionId :: MerchantCtx -> InboundEvent -> Text
scopedSessionId m ev = "whatsapp:" <> m.merchantLabel <> ":" <> ev.fromPhone

-- | Durable per-user PersonStore key (@engine.ts:1617-1620@:
-- @whatsapp:<merchantId>:<sender>@). Exposed for the webhook layer.
mkUserKey :: MerchantCtx -> InboundEvent -> Text
mkUserKey m ev = "whatsapp:" <> m.merchantLabel <> ":" <> ev.fromPhone

-- | The single entry point (webhook + golden harness both call this). Mirrors
-- @app.ts@ (resolveSession first, so saveContext persists) then
-- @engine.ts:46-494@ (allowlist gate → hydrate → intercept chain → state
-- switch). The engine reads/writes context via the SessionStore each turn.
handleMessage :: Monad m => BotEnv m -> InboundEvent -> m ()
handleMessage env ev = do
  let merchant = env.cfg.merchant
      to = ev.fromPhone
      sid = scopedSessionId merchant ev
      uk = mkUserKey merchant ev
  -- Access gate (engine.ts:60-66): only allowlisted numbers get the live flow;
  -- an empty allowlist reopens to all. Runs BEFORE any session/auth work.
  let blocked = not (null env.cfg.allowedPhones) && maybe True (`notElem` env.cfg.allowedPhones) (extractPhone ev)
  if blocked
    then void $ env.sender.sendText to comingSoonMsg
    else do
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
      s = t ctx.language
      input = T.strip (rawInput ev)
      lower = T.toLower input
  if
      | "lang:" `T.isPrefixOf` input -> handleLang env ev ctx (T.drop 5 input) -- :103-118
      | input == "choose_language" || input == "more_languages" -> handleChooseLanguage env ev ctx input -- :120-123
      | lower `elem` cancelTriggers || "cancel:" `T.isPrefixOf` input -> handleCancel env ev ctx input -- :126-129
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
      s = t ctx.language
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
-- IDLE / booking entry (engine.ts:498-531, 583-603, 714-725)
-- ---------------------------------------------------------------------------

-- | @handleIdle@ (@engine.ts:498-531@). Non-book input → intro (once) + welcome
-- menu; a book trigger with no auth → silent onboarding then booking entry.
handleIdle :: Monad m => BotEnv m -> InboundEvent -> FlowContext -> Text -> m ()
handleIdle env ev ctx input = do
  let s = t ctx.language
      to = ev.fromPhone
  if not (any (`T.isInfixOf` T.toLower input) bookTriggers)
    then do
      sendOnboardingIntroOnce env ev ctx
      row <- menuRow env ev s
      replyButtons env to s.welcome row
    else case ctx.personId of
      Nothing -> do
        let ctx1 = if ctx.pendingAction == Just PendingStatus then ctx else ctx {pendingAction = Just PendingBook}
        save env ev ctx1
        mok <- ensureAuth env ev ctx1
        case mok of
          Nothing -> pure ()
          Just ctx2 -> do
            let keepStatus = ctx2.pendingAction == Just PendingStatus
                ctx3 = if keepStatus then ctx2 else ctx2 {pendingAction = Nothing}
            unless keepStatus $ save env ev ctx3
            promptForBookingEntry env ev ctx3
      Just _ -> promptForBookingEntry env ev ctx

-- | Route a booking entrypoint (@engine.ts:583-603@). Deferred status resumes
-- first; a both-modes merchant asks which ride type, else goes straight to pickup.
promptForBookingEntry :: Monad m => BotEnv m -> InboundEvent -> FlowContext -> m ()
promptForBookingEntry env ev ctx = do
  let merchant = env.cfg.merchant
  case ctx.pendingAction of
    Just PendingStatus -> do
      let ctx1 = ctx {pendingAction = Nothing}
      save env ev ctx1
      handleStatus env ev ctx1
    _ ->
      if flexiOffered merchant && regularOffered merchant
        then sendRideTypePrompt env ev ctx
        else do
          let rt = if regularOffered merchant then Regular else Flexi
              ctx1 = ctx {rideType = Just rt} :: FlowContext
          save env ev ctx1
          promptForPickup env ev ctx1 False

-- | Ask which ride type (@engine.ts:714-725@); the @ride_type:*@ intercept handles
-- the taps.
sendRideTypePrompt :: Monad m => BotEnv m -> InboundEvent -> FlowContext -> m ()
sendRideTypePrompt env ev ctx = do
  let s = t ctx.language
      to = ev.fromPhone
  save env ev (ctx {rideType = Nothing} :: FlowContext)
  replyButtons env to s.rideTypePrompt [btn s.rideTypeFlexi "ride_type:flexi", btn s.rideTypeRegular "ride_type:regular"]

-- | The @ride_type:(flexi|regular)@ intercept (@engine.ts:276-292@): record the
-- choice, ensure auth (deferred booking survives it), then ask for pickup.
handleRideType :: Monad m => BotEnv m -> InboundEvent -> FlowContext -> Text -> m ()
handleRideType env ev ctx rt = case rt of
  "flexi" -> go Flexi
  "regular" -> go Regular
  _ -> pure ()
  where
    go rideT = do
      let ctx1 = ctx {rideType = Just rideT, pendingAction = Just PendingBook}
      save env ev ctx1
      mok <- ensureAuth env ev ctx1
      case mok of
        Nothing -> pure ()
        Just ctx2 -> do
          let ctx3 = ctx2 {pendingAction = Nothing}
          save env ev ctx3
          promptForPickup env ev ctx3 False

-- ---------------------------------------------------------------------------
-- Auth / silent onboarding (engine.ts:615-682; D2/D3)
-- ---------------------------------------------------------------------------

-- | @ensureAuth@ (@engine.ts:615-682@), 3-layer lookup ORDER preserved so the
-- golden backend-call order replays: (1) ctx.personId present → ok; (2) durable
-- PersonStore hit → adopt + refresh saved locations; (3) derive phone →
-- @authenticate@ (prod: find-or-create, no OTP — D2/D3) → getSavedLocations →
-- persist. Returns @Just ctx'@ (ok, ctx' saved) or @Nothing@ (failed; a message
-- was already sent, state reset to IDLE).
ensureAuth :: Monad m => BotEnv m -> InboundEvent -> FlowContext -> m (Maybe FlowContext)
ensureAuth env ev ctx = do
  let s = t ctx.language
      to = ev.fromPhone
      uk = mkUserKey env.cfg.merchant ev
  case ctx.personId of
    Just _ -> pure (Just ctx)
    Nothing -> do
      mstored <- env.persons.getPerson uk
      case mstored of
        Just sp -> do
          let ctx1 = ctx {personId = Just sp.personId, language = ctx.language <|> sp.language} :: FlowContext
              auth = BotAuth sp.personId
          esl <- env.backend.getSavedLocations auth
          let ctx2 = case esl of
                Right locs -> ctx1 {savedLocations = Just locs}
                Left _ -> ctx1
          save env ev ctx2
          pure (Just ctx2)
        Nothing -> case extractPhone ev of
          Nothing -> do
            reply env to s.sessionExpired
            save env ev ctx {state = Idle}
            pure Nothing
          Just phone -> do
            let ctx0 = ctx {phone = Just phone}
            eauth <- env.backend.authenticate phone
            case eauth of
              Left err -> do
                reply env to (s.setupFailed err.botErrorMessage)
                save env ev ctx0 {state = Idle}
                pure Nothing
              Right auth -> do
                esl <- env.backend.getSavedLocations auth
                let sl = either (const Nothing) Just esl
                    ctx2 = ctx0 {personId = Just auth.personId, savedLocations = sl <|> ctx0.savedLocations}
                env.persons.setPerson uk StoredPerson {personId = auth.personId, language = ctx2.language}
                save env ev ctx2
                pure (Just ctx2)

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
    let newS = t (Just l)
    row <- menuRow env ev newS
    replyButtons env to (newS.languageUpdated newS.nativeLanguageName <> newS.whatToDo) row

-- | @choose_language@ / @more_languages@ (@engine.ts:533-563@).
handleChooseLanguage :: Monad m => BotEnv m -> InboundEvent -> FlowContext -> Text -> m ()
handleChooseLanguage env ev ctx input = do
  let s = t ctx.language
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
        replyButtons env to s.selectLanguage [btn (li.nativeName <> " (" <> li.name <> ")") ("lang:" <> languageCode li.code) | li <- getAllLanguages]
      | otherwise -> pure ()

-- ---------------------------------------------------------------------------
-- Pickup (engine.ts:923-1017, 1021-1038)
-- ---------------------------------------------------------------------------

-- | Ask the rider to share their current location (@engine.ts:923-943@). The
-- metered fare line is Flexi-only and suppressed on a "Change location" re-prompt.
promptForPickup :: Monad m => BotEnv m -> InboundEvent -> FlowContext -> Bool -> m ()
promptForPickup env ev ctx suppressFare = do
  let merchant = env.cfg.merchant
      s = t ctx.language
      to = ev.fromPhone
  save env ev ctx {state = AwaitingPickup}
  let showFare = not suppressFare && ctx.rideType /= Just Regular
      fareLine = if showFare then flexiFareLine merchant ctx.language else Nothing
      body = maybe s.flexiSharePrompt (\f -> s.flexiSharePrompt <> "\n\n" <> f) fareLine
  locationRequest env to body

-- | The @__location_pin__@ intercept (@engine.ts:947-1017@). Only ever reached
-- with an 'InLocationPin'; a missing pin falls back to re-prompting.
handlePickup :: Monad m => BotEnv m -> InboundEvent -> FlowContext -> m ()
handlePickup env ev ctx = case ev.kind of
  InLocationPin lat lon mName mAddr -> handlePickupPin env ev ctx lat lon mName mAddr
  _ -> promptForPickup env ev ctx False

handlePickupPin :: Monad m => BotEnv m -> InboundEvent -> FlowContext -> Double -> Double -> Maybe Text -> Maybe Text -> m ()
handlePickupPin env ev ctx lat lon mName mAddr = do
  let merchant = env.cfg.merchant
      s = t ctx.language
      to = ev.fromPhone
  -- Serviceability geofence on the RAW pin (engine.ts:961-980), BEFORE auth/search.
  case merchant.flexiServiceArea of
    Just area | not (isWithinServiceArea lat lon area (fromMaybe 25 merchant.flexiServiceRadiusKm)) -> do
      save env ev ctx {state = Idle}
      replyButtons env to (s.flexiOutOfArea area) [btn s.flexiTryAgain "book"]
    _ -> do
      -- Ensure authenticated (silent; engine.ts:982-993).
      mctx' <-
        if isNothing ctx.personId
          then do
            let ctxP = ctx {pendingAction = Just PendingBook}
            save env ev ctxP
            mok <- ensureAuth env ev ctxP
            case mok of
              Nothing -> pure Nothing
              Just c -> do
                let c1 = c {pendingAction = Nothing}
                save env ev c1
                pure (Just c1)
          else pure (Just ctx)
      case mctx' of
        Nothing -> pure ()
        Just ctx1 -> do
          let auth = BotAuth (fromMaybe "" ctx1.personId)
          eorigin <- env.backend.reverseGeocode auth (LatLon lat lon)
          let origin0 = case eorigin of
                Right p -> p
                Left _ -> BotPlace {lat = lat, lon = lon, placeId = fmtNum lat <> "," <> fmtNum lon, address = emptyAddress}
              origin = applyNameOverride origin0 mName mAddr
              ctx2 = ctx1 {origin = Just origin}
          sendPickupConfirm env ev ctx2 mName

-- | Show the pickup confirmation (@engine.ts:1021-1038@). A named/saved place gets
-- a distinct warning that it may not be the live spot.
sendPickupConfirm :: Monad m => BotEnv m -> InboundEvent -> FlowContext -> Maybe Text -> m ()
sendPickupConfirm env ev ctx mNamedPlace = do
  let s = t ctx.language
      to = ev.fromPhone
  save env ev ctx {state = ConfirmingPickup}
  let areaMaybe = ctx.origin >>= (\o -> o.address.area)
      faMaybe = formatAddress <$> ctx.origin
      label = firstNonEmpty [mNamedPlace, areaMaybe, faMaybe] "your shared location"
      body = case mNamedPlace of
        Just np -> s.flexiConfirmSavedPlace np
        Nothing -> s.flexiConfirmPickup label
  replyButtons env to body [btn s.pickupConfirmButton "pickup_confirm", btn s.pickupAdjustButton "pickup_adjust"]

-- | @pickup_confirm@ dispatch (@engine.ts:293-305@): flexi searches immediately,
-- regular asks for a drop. Default for a bare pin: regular-only → regular, else flexi.
handlePickupConfirm :: Monad m => BotEnv m -> InboundEvent -> FlowContext -> m ()
handlePickupConfirm env ev ctx =
  let merchant = env.cfg.merchant
      rideT = case ctx.rideType of
        Just r -> r
        Nothing -> if regularOffered merchant && not (flexiOffered merchant) then Regular else Flexi
   in case rideT of
        Regular -> promptForRegularDrop env ev ctx
        Flexi -> startFlexiSearch env ev ctx

-- ---------------------------------------------------------------------------
-- Flexi flow (engine.ts:1042-1186)
-- ---------------------------------------------------------------------------

-- | The outcome of a driver-assignment poll.
data PollOutcome
  = PollAborted -- cancel landed / session reset mid-poll → exit silently
  | PollNotFound -- exhausted attempts → no auto
  | PollFound BotBookingDetails

-- | Run the metered (MeterRide) search once pickup is confirmed
-- (@engine.ts:1042-1163@): search → quotes poll → confirm → driver poll → card.
-- Fidelity note: the TS pre-confirm @saveContext(ctx)@ (:1086) is intentionally
-- OMITTED. TS relied on the in-memory store's shared-reference aliasing to carry
-- a concurrent cancel's @cancelRequested@ flag across that save; Haskell has no
-- such aliasing, so the engine instead RE-READS context at @afterConfirm@ (:1102)
-- from the store, which already reflects the cancel (state=IDLE). Net observable
-- behavior is identical (this is what makes @cancel-mid-search@ pass).
startFlexiSearch :: Monad m => BotEnv m -> InboundEvent -> FlowContext -> m ()
startFlexiSearch env ev ctx = do
  let merchant = env.cfg.merchant
      s = t ctx.language
      to = ev.fromPhone
  case (ctx.personId, ctx.origin) of
    (Just pid, Just origin) -> do
      let auth = BotAuth pid
      save env ev ctx {state = FlexiSearching, cancelRequested = Just False}
      let fareLine = flexiFareLine merchant ctx.language
          findingBody = maybe s.flexiFinding (\f -> s.flexiFinding <> "\n" <> f) fareLine
      replyButtons env to findingBody [btn s.flexiCancelSearch "cancel"]
      -- Reference time BEFORE the search (minus skew) so the booking clears listV2.
      flowStartedAt <- env.clock.now
      esid <- env.backend.searchFlexi auth origin
      case esid of
        Left _ -> flexiNoAuto env ev ctx
        Right searchId -> do
          eq <- pollFlexiQuotes env auth searchId 0
          case eq of
            Left _ -> flexiNoAuto env ev ctx
            Right [] -> flexiNoAuto env ev ctx
            Right quotes@(q0 : _) -> do
              let chosen = fromMaybe q0 (find (\q -> q.vehicleVariant == Just "AUTO_RICKSHAW") quotes)
              ebid <- env.backend.confirmQuote auth chosen.quoteId
              case ebid of
                Left _ -> flexiNoAuto env ev ctx
                Right bookingId
                  | T.null bookingId -> flexiNoAuto env ev ctx
                  | otherwise -> do
                    -- Re-read: a "Cancel search" tap can land during the blocking
                    -- search/confirm above (engine.ts:1099-1103).
                    afterConfirm <- fromMaybe initialContext <$> getCtx env ev
                    if afterConfirm.cancelRequested == Just True || afterConfirm.state == Idle
                      then pure ()
                      else do
                        let ac =
                              afterConfirm
                                { flexiSearchId = Just searchId,
                                  flexiQuoteId = Just chosen.quoteId,
                                  flexiBookingId = Just bookingId,
                                  activeBookingId = Just bookingId,
                                  selectStartedAt = Just (addUTCTime (-120) flowStartedAt),
                                  state = Tracking
                                }
                        save env ev ac
                        registerRide' env ev bookingId ac "confirmed"
                        outcome <- pollFlexiDriver env ev auth bookingId ctx.language 0
                        case outcome of
                          PollAborted -> pure ()
                          PollNotFound -> flexiNoAuto env ev ctx
                          PollFound b -> do
                            -- Claim 'assigned' so the tracker doesn't also send
                            -- this card (engine.ts:1153-1160).
                            won <- env.registry.claimStage bookingId "assigned"
                            when won $ sendFlexiDriverCard env ev b
                            bumpStage env bookingId "assigned"
    _ -> promptForPickup env ev ctx False

-- | Poll @getFlexiQuotes@ up to @flexiQuotePollAttempts@ times (@engine.ts:1074-1078@).
pollFlexiQuotes :: Monad m => BotEnv m -> BotAuth -> Text -> Int -> m (Either BotError [BotQuote])
pollFlexiQuotes env auth searchId attempt
  | attempt >= env.cfg.flexiQuotePollAttempts = pure (Right [])
  | otherwise = do
    eq <- env.backend.getFlexiQuotes auth searchId
    case eq of
      Left e -> pure (Left e)
      Right [] -> do
        env.clock.sleepMs env.cfg.flexiQuotePollIntervalMs
        pollFlexiQuotes env auth searchId (attempt + 1)
      Right qs -> pure (Right qs)

-- | Poll the KNOWN booking for driver assignment (@engine.ts:1123-1151@).
-- Re-reads context each iteration and aborts on cancel/idle (LOAD-BEARING for
-- the cancel-mid-search golden).
pollFlexiDriver :: Monad m => BotEnv m -> InboundEvent -> BotAuth -> Text -> Maybe SupportedLanguage -> Int -> m PollOutcome
pollFlexiDriver env ev auth bookingId lang attempt
  | attempt >= env.cfg.driverPollAttempts = pure PollNotFound
  | otherwise = do
    fresh <- fromMaybe initialContext <$> getCtx env ev
    if fresh.cancelRequested == Just True || fresh.state == Idle
      then pure PollAborted
      else do
        eb <- env.backend.getBookingDetails auth bookingId
        case eb of
          Right (Just b)
            | isJust b.driverName || isJust b.vehicleNumber || isJust b.rideOtp -> do
              save env ev fresh {activeBookingId = Just b.bookingId}
              pure (PollFound b)
          _ -> do
            when (attempt > 0 && attempt `mod` env.cfg.driverPollNotifyEvery == 0) $
              reply env (ev.fromPhone) ((t lang).flexiStillFinding (fmtInt (((attempt + 1) * env.cfg.driverPollIntervalMs) `div` 1000)))
            env.clock.sleepMs env.cfg.driverPollIntervalMs
            pollFlexiDriver env ev auth bookingId lang (attempt + 1)

-- | No auto available — reset to IDLE and offer a retry (@engine.ts:1178-1186@).
flexiNoAuto :: Monad m => BotEnv m -> InboundEvent -> FlowContext -> m ()
flexiNoAuto env ev ctx = do
  let s = t ctx.language
      to = ev.fromPhone
  save env ev ctx {state = Idle}
  replyButtons env to s.flexiNoAuto [btn s.flexiTryAgain "book"]

-- | Render the shared "auto found" driver card (@engine.ts:1166-1175@) via the
-- shared 'buildDriverCard' — same card the background tracker sends on a restart.
sendFlexiDriverCard :: Monad m => BotEnv m -> InboundEvent -> BotBookingDetails -> m ()
sendFlexiDriverCard env ev booking = do
  mctx <- getCtx env ev
  let lang = mctx >>= (\c -> c.language)
      card = buildDriverCard booking lang
  replyButtons env (ev.fromPhone) card.bmText card.bmButtons

-- ---------------------------------------------------------------------------
-- Regular one-way flow (engine.ts:735-919)
-- ---------------------------------------------------------------------------

-- | After pickup is confirmed, ask for the drop (@engine.ts:735-748@).
promptForRegularDrop :: Monad m => BotEnv m -> InboundEvent -> FlowContext -> m ()
promptForRegularDrop env ev ctx = do
  let s = t ctx.language
      to = ev.fromPhone
  save env ev ctx {state = AwaitingRegularDrop}
  locationRequest env to s.regularDropPrompt

-- | Capture the drop — a pin (reverse-geocode) or typed address (search →
-- disambiguate) — then price the one-way auto (@engine.ts:752-783@).
handleRegularDrop :: Monad m => BotEnv m -> InboundEvent -> FlowContext -> Text -> m ()
handleRegularDrop env ev ctx input = do
  let s = t ctx.language
      to = ev.fromPhone
  case (ctx.personId, ctx.origin) of
    (Just pid, Just _origin) -> do
      let auth = BotAuth pid
      case ev.kind of
        InLocationPin lat lon mName mAddr | input == "__location_pin__" -> do
          edest <- env.backend.reverseGeocode auth (LatLon lat lon)
          let dest0 = case edest of
                Right p -> p
                Left _ -> BotPlace {lat = lat, lon = lon, placeId = fmtNum lat <> "," <> fmtNum lon, address = emptyAddress}
              dest = applyNameOverride dest0 mName mAddr
          startRegularSearch env ev ctx {destination = Just dest}
        _ -> do
          esp <- env.backend.searchPlaces auth input (searchCenterForDest ctx)
          let places = either (const []) identity esp
          if null places
            then reply env to s.noPlacesFound
            else do
              let opts = take 3 (map (\p -> DestinationOption {description = p.description, placeId = p.placeId}) places)
              save env ev ctx {destinationOptions = Just opts, state = ConfirmingRegularDrop}
              replyButtons env to s.regularSelectDrop [btn (T.take 24 o.description) ("regdrop:" <> o.placeId) | o <- opts]
    _ -> do
      reply env to s.sessionExpired
      save env ev ctx {state = Idle}

-- | Rider picked a searched drop option (or typed a new address) (@engine.ts:786-803@).
handleConfirmingRegularDrop :: Monad m => BotEnv m -> InboundEvent -> FlowContext -> Text -> m ()
handleConfirmingRegularDrop env ev ctx input = do
  let s = t ctx.language
      to = ev.fromPhone
  if "regdrop:" `T.isPrefixOf` input
    then case ctx.personId of
      Nothing -> reply env to s.sessionExpired
      Just pid -> do
        let auth = BotAuth pid
        ep <- env.backend.getPlaceDetails auth (T.drop 8 input)
        case ep of
          Left _ -> reply env to s.somethingWentWrong
          Right place -> startRegularSearch env ev ctx {destination = Just place}
    else handleRegularDrop env ev ctx input

-- | Price the one-way auto (ONE_WAY search → estimates → pick auto) then show the
-- fare confirmation (@engine.ts:807-842@).
startRegularSearch :: Monad m => BotEnv m -> InboundEvent -> FlowContext -> m ()
startRegularSearch env ev ctx = do
  let s = t ctx.language
      to = ev.fromPhone
  case (ctx.personId, ctx.origin, ctx.destination) of
    (Just pid, Just origin, Just destination) -> do
      let auth = BotAuth pid
      save env ev ctx {state = RegularSearching}
      reply env to s.regularSearching
      esid <- env.backend.searchRide auth origin destination
      case esid of
        Left _ -> flexiNoAuto env ev ctx
        Right searchId -> do
          eest <- pollEstimates env auth searchId 0
          case eest of
            Left _ -> flexiNoAuto env ev ctx
            Right [] -> flexiNoAuto env ev ctx
            Right estimates@(e0 : _) -> do
              let autoEst = fromMaybe e0 (find (\e -> e.vehicleVariant == "AUTO_RICKSHAW") estimates)
              sendRegularFareConfirm env ev ctx {regularSearchId = Just searchId, regularEstimateId = Just autoEst.estimateId, regularFare = Just autoEst.estimatedFare}
    _ -> do
      reply env to s.sessionExpired
      save env ev ctx {state = Idle}

-- | Poll @getEstimates@ up to @regularEstimatePollAttempts@ times (@engine.ts:825-829@).
pollEstimates :: Monad m => BotEnv m -> BotAuth -> Text -> Int -> m (Either BotError [BotEstimate])
pollEstimates env auth searchId attempt
  | attempt >= env.cfg.regularEstimatePollAttempts = pure (Right [])
  | otherwise = do
    ee <- env.backend.getEstimates auth searchId
    case ee of
      Left e -> pure (Left e)
      Right [] -> do
        env.clock.sleepMs env.cfg.regularEstimatePollIntervalMs
        pollEstimates env auth searchId (attempt + 1)
      Right es -> pure (Right es)

-- | Show the auto fare + [Book / Change drop] (@engine.ts:845-857@).
sendRegularFareConfirm :: Monad m => BotEnv m -> InboundEvent -> FlowContext -> m ()
sendRegularFareConfirm env ev ctx = do
  let s = t ctx.language
      to = ev.fromPhone
  save env ev ctx {state = ConfirmingRegularFare}
  let area = firstNonEmpty [ctx.destination >>= (\d -> d.address.area), formatAddress <$> ctx.destination] "your destination"
  replyButtons
    env
    to
    (s.regularFareConfirm (fmtNum (fromMaybe 0 ctx.regularFare)) area)
    [btn s.regularConfirmButton "regular_book", btn s.regularChangeDropButton "regular_change_drop"]

-- | Book the one-way auto: select the estimate, poll for a driver, show the card,
-- register with the tracker (@engine.ts:861-919@).
confirmRegularBooking :: Monad m => BotEnv m -> InboundEvent -> FlowContext -> m ()
confirmRegularBooking env ev ctx = do
  let s = t ctx.language
      to = ev.fromPhone
  case (ctx.personId, ctx.regularEstimateId) of
    (Just pid, Just estimateId) -> do
      let auth = BotAuth pid
      save env ev ctx {cancelRequested = Just False}
      replyButtons env to s.regularBooking [btn s.flexiCancelSearch "cancel"]
      -- Reference time BEFORE select (minus skew) so the booking clears listV2.
      selectCalledAt <- addUTCTime (-120) <$> env.clock.now
      esel <- env.backend.selectEstimate auth estimateId
      case esel of
        Left _ -> flexiNoAuto env ev ctx
        Right () -> do
          afterSelect <- fromMaybe initialContext <$> getCtx env ev
          if afterSelect.cancelRequested == Just True || afterSelect.state == Idle
            then pure ()
            else do
              save env ev afterSelect {state = Tracking, selectStartedAt = Just selectCalledAt}
              outcome <- pollRegularBooking env ev auth selectCalledAt ctx.language 0
              case outcome of
                PollAborted -> pure ()
                PollNotFound -> flexiNoAuto env ev ctx
                PollFound b -> do
                  registerRide' env ev b.bookingId ctx "confirmed"
                  let hasDriver = isJust b.driverName || isJust b.rideOtp
                  when hasDriver $ do
                    _ <- env.registry.claimStage b.bookingId "assigned"
                    bumpStage env b.bookingId "assigned"
                  sendFlexiDriverCard env ev b
    _ -> reply env to s.sessionExpired

-- | Poll @getActiveBookings(createdAfter)@ for the just-booked ride
-- (@engine.ts:890-906@). Re-reads context each iteration and aborts on cancel/idle.
pollRegularBooking :: Monad m => BotEnv m -> InboundEvent -> BotAuth -> UTCTime -> Maybe SupportedLanguage -> Int -> m PollOutcome
pollRegularBooking env ev auth createdAfter lang attempt
  | attempt >= env.cfg.driverPollAttempts = pure PollNotFound
  | otherwise = do
    fresh <- fromMaybe initialContext <$> getCtx env ev
    if fresh.cancelRequested == Just True || fresh.state == Idle
      then pure PollAborted
      else do
        eb <- env.backend.getActiveBookings auth (Just createdAfter)
        case eb of
          Right (b : _) -> do
            save env ev fresh {activeBookingId = Just b.bookingId}
            pure (PollFound b)
          _ -> do
            when (attempt > 0 && attempt `mod` env.cfg.driverPollNotifyEvery == 0) $
              reply env (ev.fromPhone) ((t lang).flexiStillFinding (fmtInt (((attempt + 1) * env.cfg.driverPollIntervalMs) `div` 1000)))
            env.clock.sleepMs env.cfg.driverPollIntervalMs
            pollRegularBooking env ev auth createdAfter lang (attempt + 1)

-- ---------------------------------------------------------------------------
-- Status / tracking (engine.ts:1189-1280, 1599-1612)
-- ---------------------------------------------------------------------------

-- | @handleStatus@ (@engine.ts:1236-1280@): the rider's active ride + Call/Cancel.
handleStatus :: Monad m => BotEnv m -> InboundEvent -> FlowContext -> m ()
handleStatus env ev ctx = do
  let s = t ctx.language
      to = ev.fromPhone
      auth = BotAuth (fromMaybe "" ctx.personId)
  mb <- resolveActiveBooking env ev auth
  case mb of
    Nothing -> do
      resetContext env ev
      replyButtons env to s.noActiveRidesBook [btn s.bookARide "book"]
    Just b -> do
      let trackingLink = buildTrackingLink b
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
      auth = BotAuth (fromMaybe "" ctx.personId)
  mb <- resolveActiveBooking env ev auth
  case mb of
    Nothing -> do
      resetContext env ev
      replyButtons env to s.noActiveRidesBook [btn s.bookARide "book"]
    Just b -> do
      let rideStatusUp = maybe "" T.toUpper b.rideStatus
          statusText = if rideStatusUp == "INPROGRESS" then s.rideInProgressStatus else s.rideNotStarted
          trackingLink = buildTrackingLink b
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

-- | Resolve the rider's live booking via the durable registry + getBookingDetails
-- (@engine.ts:1599-1612@). Terminal (completed/cancelled) → Nothing.
--
-- Divergence note: TS sorts multiple registry entries newest-first by
-- @createdAt@; 'RegisteredRide' carries no @createdAt@, so we take the head
-- (typically the sole entry).
resolveActiveBooking :: Monad m => BotEnv m -> InboundEvent -> BotAuth -> m (Maybe BotBookingDetails)
resolveActiveBooking env ev auth = do
  rides <- env.registry.listByUser (mkUserKey env.cfg.merchant ev)
  case listToMaybe rides of
    Nothing -> pure Nothing
    Just r -> do
      eb <- env.backend.getBookingDetails auth r.bookingId
      case eb of
        Right (Just b)
          | classifyStage b `notElem` [StageCompleted, StageCancelled] -> pure (Just b)
        _ -> pure Nothing

-- | Build a tracking link (@engine.ts:1471-1477@). Fidelity note: 'MerchantCtx'
-- has no @nyTrackingUrl@ and 'BotBookingDetails' no separate rideId, so we use the
-- default template with the bookingId. (The link text is not golden-projected.)
buildTrackingLink :: BotBookingDetails -> Text
buildTrackingLink b = T.replace "{rideId}" b.bookingId "https://www.nammayatri.in/u?vp=shareRide&rideId={rideId}"

-- ---------------------------------------------------------------------------
-- Cancel (engine.ts:1282-1355, 153-178)
-- ---------------------------------------------------------------------------

-- | @handleCancel@ (@engine.ts:1282-1355@): flag a cancel, read the active
-- booking, branch on its status (completed/cancelled/in-progress/other), and on a
-- real cancellable booking call @cancelRide@ + stop the tracker.
handleCancel :: Monad m => BotEnv m -> InboundEvent -> FlowContext -> Text -> m ()
handleCancel env ev ctx input = do
  let s = t ctx.language
      to = ev.fromPhone
      explicitBookingId = if "cancel:" `T.isPrefixOf` input then Just (T.drop 7 input) else Nothing
  case ctx.personId of
    Nothing -> do
      resetContext env ev
      replyWithMenu env ev s.cancelled
    Just pid -> do
      let auth = BotAuth pid
      save env ev ctx {cancelRequested = Just True}
      createdAfter <- case ctx.selectStartedAt of
        Just tt -> pure tt
        Nothing -> addUTCTime (-86400) <$> env.clock.now
      eb <- env.backend.getActiveBookings auth (Just createdAfter)
      let bookings = either (const []) identity eb
          booking = case explicitBookingId of
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
            replyWithMenu env ev s.rideCompleted
          | bStatus == "CANCELLED" || rStatus == "CANCELLED" -> do
            case trackedId of
              Just tid -> do
                _ <- env.registry.claimStage tid "cancelled"
                env.registry.removeRide tid
              Nothing -> pure ()
            resetContext env ev
            replyWithMenu env ev s.rideAlreadyCancelled
          | rStatus == "INPROGRESS" -> do
            save env ev ctx {cancelRequested = Just False}
            reply env to s.rideInProgress
          | otherwise -> case booking of
            Just b -> do
              ec <- env.backend.cancelRide auth b.bookingId b.bookingStatus
              case ec of
                Left err -> do
                  resetContext env ev
                  replyWithMenu env ev (s.cancelFailed err.botErrorMessage)
                Right () -> do
                  let stopId = fromMaybe b.bookingId trackedId
                  _ <- env.registry.claimStage stopId "cancelled"
                  env.registry.removeRide stopId
                  resetContext env ev
                  replyWithMenu env ev s.rideCancelled
            Nothing -> do
              resetContext env ev
              replyWithMenu env ev s.cancelled

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
        let auth = BotAuth (fromMaybe "" ctx.personId)
        createdAfter <- case ctx.selectStartedAt of
          Just tt -> pure tt
          Nothing -> addUTCTime (-86400) <$> env.clock.now
        eb <- env.backend.getActiveBookings auth (Just createdAfter)
        case eb of
          Right bookings -> do
            let booking = find (\b -> b.bookingId == bookingId) bookings <|> listToMaybe bookings
            case booking >>= (\b -> b.driverName) of
              Just dn -> pure (s.cancelConfirmWithDriver dn (booking >>= (\b -> b.vehicleNumber)))
              Nothing -> pure s.cancelConfirm
          Left _ -> pure s.cancelConfirm
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
  eb <- env.backend.getActiveBookings auth ctx.selectStartedAt
  let bookings = either (const []) identity eb
  case bookings of
    (b : _) -> do
      -- rideId: BotBookingDetails exposes no separate ride id, so use bookingId.
      esos <- env.backend.triggerSOS auth b.bookingId Nothing
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
-- More / help / support / call-driver / end-OTP (engine.ts:365-406, 325-359)
-- ---------------------------------------------------------------------------

-- | "More options" submenu (@engine.ts:365-375@).
handleMore :: Monad m => BotEnv m -> InboundEvent -> FlowContext -> m ()
handleMore env ev ctx = do
  let s = t ctx.language
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
  let s = t ctx.language
  row <- menuRow env ev s
  replyButtons env (ev.fromPhone) s.moreTitle row

-- | Support contact (@engine.ts:381-387@), then loop to menu.
handleSupport :: Monad m => BotEnv m -> InboundEvent -> FlowContext -> m ()
handleSupport env ev ctx = do
  let s = t ctx.language
      to = ev.fromPhone
      raw = fromMaybe "" env.cfg.merchant.flexiSupportPhone
      dial = fromMaybe raw (formatDialable (Just raw))
  reply env to (s.supportMessage dial)
  row <- menuRow env ev s
  replyButtons env to s.moreTitle row

-- | @call_driver@ (@engine.ts:389-406@): reveal the driver's dial number.
handleCallDriver :: Monad m => BotEnv m -> InboundEvent -> FlowContext -> m ()
handleCallDriver env ev ctx = do
  let s = t ctx.language
      to = ev.fromPhone
      auth = BotAuth (fromMaybe "" ctx.personId)
  eb <- env.backend.getActiveBookings auth ctx.selectStartedAt
  let bookings = either (const []) identity eb
  case bookings of
    (b : _) -> case b.driverNumber of
      Just phone -> reply env to (s.driverPhone phone)
      Nothing -> reply env to s.driverDetailsNotAvailable
    [] -> reply env to s.noActiveRide

-- | @flexi_end_otp:<id>@ (@engine.ts:325-359@): reveal the rental end OTP on
-- demand, with a path-injection guard + registry ownership check (IDOR defense).
handleFlexiEndOtp :: Monad m => BotEnv m -> InboundEvent -> FlowContext -> Text -> m ()
handleFlexiEndOtp env ev ctx bid = do
  let s = t ctx.language
      to = ev.fromPhone
      uk = mkUserKey env.cfg.merchant ev
  if isNothing ctx.personId || not (isValidBookingId bid)
    then reply env to s.sessionExpired
    else do
      mowned <- env.registry.getRide bid
      case mowned of
        Just owned | owned.userKey == uk -> do
          let auth = BotAuth (fromMaybe "" ctx.personId)
          eb <- env.backend.getBookingDetails auth bid
          case eb of
            Right (Just b) -> do
              let status = T.toUpper (fromMaybe b.bookingStatus b.rideStatus)
              if status == "COMPLETED" || status == "CANCELLED"
                then reply env to s.flexiRideAlreadyEnded
                else case b.endOtp of
                  Just eo -> replyButtons env to (s.flexiEndOtpShare eo) [btn s.flexiEndRideButton ("flexi_end_otp:" <> bid)]
                  Nothing -> reply env to s.flexiEndOtpNotReady
            _ -> replyButtons env to s.flexiEndOtpFetchError [btn s.flexiEndRideButton ("flexi_end_otp:" <> bid)]
        _ -> reply env to s.flexiRideAlreadyEnded

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
  let s = t ctx.language
      to = ev.fromPhone
  case env.cfg.merchant.flexiIntroVideoUrl of
    Just url -> void $ env.sender.sendVideo to url (Just s.howItWorksCaption)
    Nothing -> pure ()

-- | "How it works" explainer (@engine.ts:1586-1594@): intro video + text steps.
sendHowItWorks :: Monad m => BotEnv m -> InboundEvent -> FlowContext -> m ()
sendHowItWorks env ev ctx = do
  sendIntroVideo env ev ctx
  reply env (ev.fromPhone) ((t ctx.language).howItWorksText)

-- ---------------------------------------------------------------------------
-- Registry / menu / context helpers
-- ---------------------------------------------------------------------------

-- | Register a booking with the durable ride tracker (@engine.ts:1498-1515@).
registerRide' :: Monad m => BotEnv m -> InboundEvent -> Text -> FlowContext -> Text -> m ()
registerRide' env ev bookingId ctx stage =
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
        lastStage = Just stage
      }

-- | Advance the persisted last-pushed stage on a registry entry (@ride.update@).
bumpStage :: Monad m => BotEnv m -> Text -> Text -> m ()
bumpStage env bid stage = do
  mr <- env.registry.getRide bid
  case mr of
    Just r -> env.registry.updateRide r {lastStage = Just stage}
    Nothing -> pure ()

-- | The context-aware menu row (@engine.ts:1530-1546@): [Track?] · Book · More ·
-- Language (Track shown only when a ride is live). Kept to <=4 buttons.
menuRow :: Monad m => BotEnv m -> InboundEvent -> LanguageStrings -> m [OutButton]
menuRow env ev s = do
  let merchant = env.cfg.merchant
      uk = mkUserKey merchant ev
  has <- env.registry.hasActiveRide uk
  let trackBtn = [btn s.trackRide "status" | has]
      bookBtn =
        if flexiOffered merchant
          then btn s.rideTypeFlexi "ride_type:flexi"
          else btn s.bookARide "ride_type:regular"
  pure (trackBtn <> [bookBtn, btn s.moreButton "more", btn s.chooseLanguage "choose_language"])

-- | @replyWithMenu@ (@engine.ts:1357-1364@): a prefix + "what to do" + menu row.
replyWithMenu :: Monad m => BotEnv m -> InboundEvent -> Text -> m ()
replyWithMenu env ev prefix = do
  mctx <- getCtx env ev
  let s = t (mctx >>= (\c -> c.language))
  row <- menuRow env ev s
  replyButtons env (ev.fromPhone) (prefix <> s.whatToDo) row

-- | @resetContext@ (@engine.ts:1367-1374@): back to INITIAL, preserving personId,
-- saved locations, and language.
resetContext :: Monad m => BotEnv m -> InboundEvent -> m ()
resetContext env ev = do
  mctx <- getCtx env ev
  let ctx = fromMaybe initialContext mctx
  save env ev initialContext {personId = ctx.personId, savedLocations = ctx.savedLocations, language = ctx.language}

-- ---------------------------------------------------------------------------
-- Small pure/effect helpers
-- ---------------------------------------------------------------------------

reply :: Monad m => BotEnv m -> Text -> Text -> m ()
reply env to' body = void $ env.sender.sendText to' body

replyButtons :: Monad m => BotEnv m -> Text -> Text -> [OutButton] -> m ()
replyButtons env to' body btns = void $ env.sender.sendButtons to' body btns

locationRequest :: Monad m => BotEnv m -> Text -> Text -> m ()
locationRequest env to' body = void $ env.sender.sendLocationRequest to' body

save :: Monad m => BotEnv m -> InboundEvent -> FlowContext -> m ()
save env ev ctx = env.sessions.saveContext (scopedSessionId env.cfg.merchant ev) ctx

getCtx :: Monad m => BotEnv m -> InboundEvent -> m (Maybe FlowContext)
getCtx env ev = env.sessions.getContext (scopedSessionId env.cfg.merchant ev)

btn :: Text -> Text -> OutButton
btn title bid = OutButton {btnId = bid, btnTitle = title, btnDesc = Nothing}

-- | The raw message text the intercept chain matches on: text body, button/list
-- reply id, or the @__location_pin__@ sentinel (@whatsapp.ts@ parse; engine.ts
-- consumes @message.text.trim()@).
rawInput :: InboundEvent -> Text
rawInput ev = case ev.kind of
  InText txt -> txt
  InButtonTap bid -> bid
  InLocationPin {} -> "__location_pin__"

-- | First non-empty of the options, else the default (mirrors TS @a || b || …@).
firstNonEmpty :: [Maybe Text] -> Text -> Text
firstNonEmpty xs d = fromMaybe d (listToMaybe [x | Just x <- xs, not (T.null x)])

-- | Format a place address (@engine.ts:27-31@): @building, street, area@ (non-empty),
-- else @"lat, lon"@.
formatAddress :: BotPlace -> Text
formatAddress p =
  let parts = [x | Just x <- [p.address.building, p.address.street, p.address.area], not (T.null x)]
   in if null parts then fmtNum p.lat <> ", " <> fmtNum p.lon else T.intercalate ", " parts

-- | If a shared name/address is present and the resolved place has no area, adopt
-- it (@engine.ts:1009-1011,768@).
applyNameOverride :: BotPlace -> Maybe Text -> Maybe Text -> BotPlace
applyNameOverride p mName mAddr = case mName <|> mAddr of
  Just nm | isNothing p.address.area -> p {address = (p.address) {area = Just nm}}
  _ -> p

emptyAddress :: BotAddress
emptyAddress = BotAddress {area = Nothing, building = Nothing, city = Nothing, country = Nothing, state = Nothing, street = Nothing}

-- | The metered-tariff display line, or Nothing when unset (@engine.ts:571-579@).
flexiFareLine :: MerchantCtx -> Maybe SupportedLanguage -> Maybe Text
flexiFareLine merchant lang = case (merchant.flexiBaseFare, merchant.flexiPerKm) of
  (Just base, Just perKm) -> Just ((t lang).flexiFareRate (fmtNum base) (fmtNum perKm))
  _ -> Nothing

flexiOffered :: MerchantCtx -> Bool
flexiOffered merchant = merchant.flexiEnabled

regularOffered :: MerchantCtx -> Bool
regularOffered merchant = merchant.regularEnabled

-- | Autocomplete search center for a destination search (@engine.ts:1660-1672@):
-- the confirmed origin, else the first saved location, else Nothing (client default).
searchCenterForDest :: FlowContext -> Maybe LatLon
searchCenterForDest ctx = case ctx.origin of
  Just o -> Just (LatLon o.lat o.lon)
  Nothing -> case ctx.savedLocations of
    Just (l : _) -> Just (LatLon l.lat l.lon)
    _ -> Nothing

-- | Extract a normalized 10-digit phone from the channel (@engine.ts:1647-1654@):
-- strip non-digits, drop a leading @91@ when longer than 10, require exactly 10.
extractPhone :: InboundEvent -> Maybe Text
extractPhone ev =
  let digits = T.filter isDigit ev.fromPhone
      d = if "91" `T.isPrefixOf` digits && T.length digits > 10 then T.drop 2 digits else digits
   in if T.length d == 10 then Just d else Nothing

-- | A server-generated booking id: @^[A-Za-z0-9_-]+$@ (@engine.ts:330@) — rejects
-- path-traversal chars a user could TYPE into the @flexi_end_otp:@ prefix.
isValidBookingId :: Text -> Bool
isValidBookingId b = not (T.null b) && T.all ok b
  where
    ok c = isAsciiUpper c || isAsciiLower c || isDigit c || c == '-' || c == '_'

bookTriggers :: [Text]
bookTriggers = ["book", "ride", "cab", "auto", "book a ride", "book ride"]

cancelTriggers :: [Text]
cancelTriggers = ["cancel", "stop", "exit", "quit", "reset"]

statusTriggers :: [Text]
statusTriggers = ["status", "track", "where is my ride"]

-- | Bilingual "coming soon" for a non-allowlisted number (@engine.ts:63@).
comingSoonMsg :: Text
comingSoonMsg =
  "🙏 Namma Yatri WhatsApp booking is coming soon. Please check back later.\n\nಇದು ಶೀಘ್ರದಲ್ಲೇ ಬರಲಿದೆ. ದಯವಿಟ್ಟು ನಂತರ ಪ್ರಯತ್ನಿಸಿ."

-- | Emergency helpline reply (@engine.ts:261@).
emergencyMsg :: Text
emergencyMsg = "\128222 Emergency helpline: *112*\n\nPlease call 112 directly for immediate assistance."
