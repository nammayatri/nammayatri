-- | The ride-booking flow: how a booking gets CREATED.
--
-- Entry and pickup capture (shared by both products), then the two product
-- paths — flexi (rental-style, no destination) and regular (destination +
-- fare confirmation). Ride lifecycle AFTER creation lives in
-- "WhatsappBot.Ride", which this module uses and does not duplicate.
--
-- A second flow (e.g. hotspot-OTP booking) sits BESIDE this module and reuses
-- WhatsappBot.Ride; it does not import this one.
--
-- 'menuRow' lives HERE and not in "WhatsappBot.Env" because it is not
-- flow-agnostic: it hardcodes this flow's entry wire strings
-- @"ride_type:flexi"@ / @"ride_type:regular"@ and its copy, and it calls
-- 'flexiOffered' to pick between them. The two consumers of the menu row that
-- must NOT depend on this module — @Env.replyWithMenu@ and
-- @Ride.handleCancel@ — take the row as a PARAMETER (the same hook technique
-- 'ensureAuth' uses for its post-auth prefetch), which is what keeps the edges
-- @Engine -> Flow.Booking -> {Ride, Env}@ acyclic. A second flow supplies its
-- own row.
--
-- This module must NEVER import @WhatsappBot.Engine@, and must NEVER be
-- imported by @WhatsappBot.Ride@ or @WhatsappBot.Env@.
module WhatsappBot.Flow.Booking
  ( -- * Booking entry
    promptForBookingEntry,
    sendRideTypePrompt,
    handleRideType,
    bookTriggers,

    -- * Pickup capture (shared by both products)
    promptForPickup,
    handlePickup,
    sendPickupConfirm,
    handlePickupConfirm,

    -- * Regular one-way (destination + fare confirmation)
    promptForRegularDrop,
    handleRegularDrop,
    handleConfirmingRegularDrop,
    sendRegularFareConfirm,
    confirmRegularBooking,

    -- * Flexi (metered, no destination)
    handleFlexiEndOtp,

    -- * The menu row + merchant capabilities (booking-specific wire strings)
    menuRow,
    flexiOffered,
    regularOffered,
    regularButton,

    -- * The flow's post-auth prefetch hook
    prefetchSavedLocations,
  )
where

import Control.Applicative ((<|>))
import qualified Data.Text as T
import Data.Time (addUTCTime)
import Kernel.Prelude
import WhatsappBot.Cities (isWithinServiceArea)
-- The flow-agnostic conversation primitives live in Env (the dependency edge
-- runs Flow.Booking -> Env and never back).
import WhatsappBot.Env (BotEnv, btn, ensureAuth, firstNonEmpty, getCtx, locationRequest, mkUserKey, reply, replyButtons, save)
import WhatsappBot.I18n (LanguageStrings, SupportedLanguage, t)
-- Instances only: LanguageStrings / LanguageInfo are dot-accessed via RDP
-- (getField), so their selectors are never referenced by name, but the HasField
-- instances are needed. (Naming them would trip -Wunused-imports under -Werror.)
import WhatsappBot.I18n.Types ()
import WhatsappBot.Messages (buildDriverCard)
-- The flow-agnostic ride lifecycle lives in Ride (Flow.Booking -> Ride -> Env).
import WhatsappBot.Ride (BookingWindow (..), bumpStage, handleStatus, isValidBookingId, listedBookings, registerRide')
import WhatsappBot.Types
import WhatsappBot.Util (fmtInt, fmtNum)

-- ---------------------------------------------------------------------------
-- Booking entry (engine.ts:583-603, 714-725, 276-292)
-- ---------------------------------------------------------------------------

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
      mok <- ensureAuth env ev (prefetchSavedLocations env) ctx1
      case mok of
        Nothing -> pure ()
        Just ctx2 -> do
          let ctx3 = ctx2 {pendingAction = Nothing}
          save env ev ctx3
          promptForPickup env ev ctx3 False

-- ---------------------------------------------------------------------------
-- Post-auth prefetch (engine.ts:615-682; D2/D3)
-- ---------------------------------------------------------------------------

-- | The booking flow's post-auth prefetch — the @getSavedLocations@ call that
-- used to be inline in 'ensureAuth'. Kept as a named binding so the golden
-- backend-call order (@authenticate → getSavedLocations@) is visible at the
-- call site rather than buried in the auth ladder.
prefetchSavedLocations :: Monad m => BotEnv m -> BotAuth -> FlowContext -> m FlowContext
prefetchSavedLocations env auth ctx = do
  esl <- env.backend.getSavedLocations auth
  pure $ case esl of
    Right locs -> ctx {savedLocations = Just locs} :: FlowContext
    Left _ -> ctx

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
            mok <- ensureAuth env ev (prefetchSavedLocations env) ctxP
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
                                { flexiBookingId = Just bookingId,
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
--
-- MISNOMER, deliberately preserved: despite the @flexi@ prefix this is the
-- shared "no auto available" terminus for BOTH products — the REGULAR path
-- calls it five times ('startRegularSearch' ×3, 'confirmRegularBooking' ×2), as
-- the TS did. Renaming it is out of scope for a pure move: the copy it sends
-- (@s.flexiNoAuto@) and the @"book"@ button id are wire/copy contracts, and the
-- name is what the @*.ts:NNN@ citations resolve against.
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
              save env ev ctx {state = ConfirmingRegularDrop}
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
              sendRegularFareConfirm env ev ctx {regularEstimateId = Just autoEst.estimateId, regularFare = Just autoEst.estimatedFare}
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
        -- The booking is not in the ledger yet (registerRide' runs only once the
        -- poll finds it), so this is the listing path — with the reference time
        -- captured before selectEstimate, threaded through rather than read back
        -- out of the session.
        bookings <- listedBookings env auth fresh (SinceExactly createdAfter)
        case bookings of
          (b : _) -> do
            save env ev fresh {activeBookingId = Just b.bookingId}
            pure (PollFound b)
          [] -> do
            when (attempt > 0 && attempt `mod` env.cfg.driverPollNotifyEvery == 0) $
              reply env (ev.fromPhone) ((t lang).flexiStillFinding (fmtInt (((attempt + 1) * env.cfg.driverPollIntervalMs) `div` 1000)))
            env.clock.sleepMs env.cfg.driverPollIntervalMs
            pollRegularBooking env ev auth createdAfter lang (attempt + 1)

-- ---------------------------------------------------------------------------
-- Flexi end-OTP (engine.ts:325-359)
-- ---------------------------------------------------------------------------

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
      -- NOT 'Ride.currentRide'. This is an AUTHORIZATION check on the specific,
      -- USER-SUPPLIED @bid@ — "does the ride this person typed belong to them?"
      -- — not a resolution of "the current ride". Routing it through the
      -- ledger's newest-entry lookup would silently change what is being
      -- authorised and defeat the IDOR defence.
      mowned <- env.registry.getRide bid
      case mowned of
        Just owned | owned.userKey == uk -> do
          -- Non-empty: the enclosing guard already rejected @isNothing ctx.personId@.
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
-- The menu row + merchant capabilities (engine.ts:1530-1546)
-- ---------------------------------------------------------------------------

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

flexiOffered :: MerchantCtx -> Bool
flexiOffered merchant = merchant.flexiEnabled

regularOffered :: MerchantCtx -> Bool
regularOffered merchant = merchant.regularEnabled

-- | The "switch to Regular" button — owns the @"ride_type:regular"@ wire
-- string, same reasoning as 'menuRow': a caller outside this module (e.g.
-- "WhatsappBot.Engine") asks for the button instead of writing the booking
-- wire string itself.
regularButton :: LanguageStrings -> OutButton
regularButton s = btn s.rideTypeRegular "ride_type:regular"

-- ---------------------------------------------------------------------------
-- Small pure/effect helpers
-- ---------------------------------------------------------------------------

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

-- | Autocomplete search center for a destination search (@engine.ts:1660-1672@):
-- the confirmed origin, else the first saved location, else Nothing (client default).
searchCenterForDest :: FlowContext -> Maybe LatLon
searchCenterForDest ctx = case ctx.origin of
  Just o -> Just (LatLon o.lat o.lon)
  Nothing -> case ctx.savedLocations of
    Just (l : _) -> Just (LatLon l.lat l.lon)
    _ -> Nothing

bookTriggers :: [Text]
bookTriggers = ["book", "ride", "cab", "auto", "book a ride", "book ride"]
