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

    -- * Favourite places (list at pickup/drop; save steps live in Flow.Favourites)
    handleFavouritePick,
    shareNewLocation,
    resumeAfterFavourite,

    -- * Regular one-way (destination + fare confirmation)
    promptForRegularDrop,
    handleRegularDrop,
    handleConfirmingRegularDrop,
    sendRegularFareConfirm,
    confirmRegularBooking,

    -- * Flexi (metered, no destination)
    handleFlexiEndOtp,
    flexiFareLine,

    -- * The menu row + merchant capabilities (booking-specific wire strings)
    menuRow,
    flexiOffered,
    regularOffered,

    -- * Ride-type chooser button layout (initial prompt + "More" reveal)
    rideTypeButtons,
    hiddenRideTypeButtons,

    -- * The flow's post-auth prefetch hook
    prefetchSavedLocations,
  )
where

import Control.Applicative ((<|>))
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Time (addUTCTime)
import Kernel.Prelude
-- The flow-agnostic conversation primitives live in Env (the dependency edge
-- runs Flow.Booking -> Env and never back).
import WhatsappBot.Env (BotEnv, btn, ensureAuth, firstNonEmpty, getCtx, locationRequest, mkUserKey, reply, replyButtons, replyWithMenu, save)
import qualified WhatsappBot.Env as Fields
import WhatsappBot.Flow.Favourites (favourites, findFavourite, isSavedPlace, savedToPlace, sendPlaceList)
import qualified WhatsappBot.Handles as Fields
import WhatsappBot.I18n (LanguageStrings, SupportedLanguage, t)
import qualified WhatsappBot.I18n as Fields
-- Instances only: LanguageStrings / LanguageInfo are dot-accessed via RDP
-- (getField), so their selectors are never referenced by name, but the HasField
-- instances are needed. (Naming them would trip -Wunused-imports under -Werror.)
import WhatsappBot.I18n.Types ()
import WhatsappBot.Messages (buildDriverCard)
import qualified WhatsappBot.Messages as Fields
-- The flow-agnostic ride lifecycle lives in Ride (Flow.Booking -> Ride -> Env).
import WhatsappBot.Ride (BookingWindow (..), bumpStage, handleStatus, isValidBookingId, listedBookings, registerRide')
import WhatsappBot.Types
import WhatsappBot.Util (fmtInt, fmtNum, placeOrPin)
import qualified WhatsappBot.Types as BotAddress (BotAddress (..))
import qualified WhatsappBot.Types as BotPlace (BotPlace (..))
import qualified WhatsappBot.Types as FlowContext (FlowContext (..))

-- ---------------------------------------------------------------------------
-- Booking entry (engine.ts:583-603, 714-725, 276-292)
-- ---------------------------------------------------------------------------

-- | Route a booking entrypoint (@engine.ts:583-603@). Deferred status resumes
-- first; a both-modes merchant asks which ride type, else goes straight to pickup.
promptForBookingEntry :: Monad m => BotEnv m -> InboundEvent -> FlowContext -> m ()
promptForBookingEntry env ev ctx = do
  let merchant = env.cfg.merchant
      s = t env.cfg.translations ctx.language
  case ctx.pendingAction of
    Just PendingStatus -> do
      let ctx1 = ctx {FlowContext.pendingAction = Nothing}
      save env ev ctx1
      handleStatus env ev ctx1
    _
      -- An empty/all-unrecognized rideTypesOrder (misconfigured merchant --
      -- every entry failed to parse, see Adapter.Env.parseRideType) must NOT
      -- silently fall through to the Flexi default below: flexiOffered and
      -- regularOffered are both False in exactly this case, so it's caught
      -- here, before the flexi/regular split, instead of being folded into
      -- "otherwise" and booking a ride type the merchant never offered.
      | null merchant.rideTypesOrder -> reply env ev.fromPhone s.somethingWentWrong
      -- Count-based, not name-based: was `flexiOffered merchant &&
      -- regularOffered merchant`, which only ever asks "is it exactly these
      -- two named types" -- equivalent today (only 2 real types exist), but
      -- breaks the moment a 3rd type is added (a merchant offering
      -- [Flexi, ThirdType] would wrongly skip the chooser). Length naturally
      -- scales to any number of ride types without this line changing again.
      | length merchant.rideTypesOrder > 1 -> sendRideTypePrompt env ev ctx
      | otherwise -> do
        let rt = if regularOffered merchant then Regular else Flexi
            ctx1 = ctx {FlowContext.rideType = Just rt} :: FlowContext
        save env ev ctx1
        promptForPickup env ev ctx1

-- | Ask which ride type (@engine.ts:714-725@); the @ride_type:*@ intercept handles
-- the taps, and the @more_ride_types@ intercept (Engine.hs) handles a tap on
-- "More" when the merchant offers more types than fit as direct buttons —
-- see 'rideTypeButtons'.
sendRideTypePrompt :: Monad m => BotEnv m -> InboundEvent -> FlowContext -> m ()
sendRideTypePrompt env ev ctx = do
  let s = t env.cfg.translations ctx.language
      to = ev.fromPhone
      merchant = env.cfg.merchant
  save env ev (ctx {FlowContext.rideType = Nothing} :: FlowContext)
  replyButtons env to s.rideTypePrompt (rideTypeButtons s merchant.maxDirectButtons merchant.rideTypesOrder)

-- | The @ride_type:(flexi|regular)@ intercept (@engine.ts:276-292@): record the
-- choice, ensure auth (deferred booking survives it), then ask for pickup.
handleRideType :: Monad m => BotEnv m -> InboundEvent -> FlowContext -> Text -> m ()
handleRideType env ev ctx rt = case rt of
  "flexi" -> go Flexi
  "regular" -> go Regular
  _ -> pure ()
  where
    go rideT = do
      let ctx1 = ctx {FlowContext.rideType = Just rideT, FlowContext.pendingAction = Just PendingBook}
      save env ev ctx1
      mok <- ensureAuth env ev (prefetchSavedLocations env) ctx1
      case mok of
        Nothing -> pure ()
        Just ctx2 -> do
          let ctx3 = ctx2 {FlowContext.pendingAction = Nothing}
          save env ev ctx3
          promptForPickup env ev ctx3

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
    Right locs -> ctx {FlowContext.savedLocations = Just locs} :: FlowContext
    Left _ -> ctx

-- ---------------------------------------------------------------------------
-- Pickup (engine.ts:923-1017, 1021-1038)
-- ---------------------------------------------------------------------------

-- | Ask the rider to share their current location (@engine.ts:923-943@). No fare
-- line here: the rate now comes from the search's quote, which doesn't exist yet.
-- A rider with favourites gets the place list instead of the location request
-- (WhatsApp's location request can't carry other options).
promptForPickup :: Monad m => BotEnv m -> InboundEvent -> FlowContext -> m ()
promptForPickup env ev ctx = promptForPickupWith env ev ctx Nothing

promptForPickupWith :: Monad m => BotEnv m -> InboundEvent -> FlowContext -> Maybe Text -> m ()
promptForPickupWith env ev ctx0 mNotice = do
  ctx <- loadFavouritesIfMissing env ctx0
  let s = t env.cfg.translations ctx.language
      to = ev.fromPhone
      body = withNotice mNotice s.flexiSharePrompt
  save env ev ctx {FlowContext.state = AwaitingPickup}
  if not env.cfg.merchant.favouritesEnabled
    then locationRequest env to body
    else case favourites ctx of
      [] -> locationRequestWithNudge env ev ctx s body
      favs -> sendPlaceList env to s body s.favChoosePickup favs

-- | The @__location_pin__@ intercept (@engine.ts:947-1017@). Only ever reached
-- with an 'InLocationPin'; a missing pin falls back to re-prompting.
handlePickup :: Monad m => BotEnv m -> InboundEvent -> FlowContext -> m ()
handlePickup env ev ctx = case ev.kind of
  InLocationPin lat lon mName mAddr -> handlePickupPin env ev ctx lat lon mName mAddr
  _ -> promptForPickup env ev ctx

handlePickupPin :: Monad m => BotEnv m -> InboundEvent -> FlowContext -> Double -> Double -> Maybe Text -> Maybe Text -> m ()
handlePickupPin env ev ctx lat lon mName mAddr =
  withPickupChecks env ev ctx (LatLon lat lon) $ \ctx1 auth -> continuePickup env ev ctx1 auth lat lon mName mAddr

-- | Auth, then the serviceability geofence, for a pickup point; runs @k@ when
-- both pass. Shared by a shared pin and a picked favourite.
withPickupChecks :: Monad m => BotEnv m -> InboundEvent -> FlowContext -> LatLon -> (FlowContext -> BotAuth -> m ()) -> m ()
withPickupChecks env ev ctx point k = do
  let merchant = env.cfg.merchant
      to = ev.fromPhone
  -- Auth now runs BEFORE the geofence check (reordered from the original
  -- engine.ts:961-980 ordering, where the geofence ran first): checkServiceable
  -- needs a real personId, unlike the old pure isWithinServiceArea/Cities.hs
  -- lookup. Real tradeoff: a person record now gets created even for someone
  -- who turns out to be out-of-area, which the original ordering specifically
  -- avoided — accepted in exchange for a real DB-backed geofence check instead
  -- of a hardcoded-city-list approximation.
  mctx' <-
    if isNothing ctx.personId
      then do
        let ctxP = ctx {FlowContext.pendingAction = Just PendingBook}
        save env ev ctxP
        mok <- ensureAuth env ev (prefetchSavedLocations env) ctxP
        case mok of
          Nothing -> pure Nothing
          Just c -> do
            let c1 = c {FlowContext.pendingAction = Nothing}
            save env ev c1
            pure (Just c1)
      else pure (Just ctx)
  case mctx' of
    Nothing -> pure ()
    Just ctx1 -> do
      let auth = BotAuth (fromMaybe "" ctx1.personId)
          s = t env.cfg.translations ctx1.language
      -- Serviceability geofence, now via the real DB-backed checkServiceable
      -- handle call instead of the pure hardcoded-city-list lookup.
      case merchant.flexiServiceArea of
        Nothing -> k ctx1 auth
        Just area -> do
          eServiceable <- env.backend.checkServiceable auth point
          case eServiceable of
            Right True -> k ctx1 auth
            _ -> do
              save env ev ctx1 {FlowContext.state = Idle}
              replyButtons env to (s.flexiOutOfArea area) [btn s.flexiTryAgain "book"]

-- | The post-auth, post-geofence continuation of handlePickupPin: reverse-
-- geocode the pin and send the pickup confirmation. Split out because the
-- reorder above means this now runs from inside the geofence-passed branch
-- instead of directly after auth.
continuePickup :: Monad m => BotEnv m -> InboundEvent -> FlowContext -> BotAuth -> Double -> Double -> Maybe Text -> Maybe Text -> m ()
continuePickup env ev ctx1 auth lat lon mName mAddr = do
  eorigin <- env.backend.reverseGeocode auth (LatLon lat lon)
  let origin0 = placeOrPin lat lon eorigin
      origin = applyNameOverride origin0 mName mAddr
  continuePickupAt env ev ctx1 origin mName

-- | The pickup is known: regular asks for the drop, flexi confirms the pickup.
continuePickupAt :: Monad m => BotEnv m -> InboundEvent -> FlowContext -> BotPlace -> Maybe Text -> m ()
continuePickupAt env ev ctx1 origin mName = do
  let ctx2 = ctx1 {FlowContext.origin = Just origin}
  -- Regular asks for the drop straight away: its fare confirmation shows the full
  -- pickup address and has a Change pickup button, and nothing is booked before
  -- Book auto. Flexi keeps the confirm step, because confirming pickup IS its booking.
  case resolvedRideType env.cfg.merchant ctx2 of
    Regular -> promptForRegularDrop env ev ctx2
    Flexi -> sendPickupConfirm env ev ctx2 mName

-- | Show the pickup confirmation (@engine.ts:1021-1038@). A named/saved place gets
-- a distinct warning that it may not be the live spot.
sendPickupConfirm :: Monad m => BotEnv m -> InboundEvent -> FlowContext -> Maybe Text -> m ()
sendPickupConfirm env ev ctx = sendPickupConfirmWith env ev ctx Nothing

-- | 'sendPickupConfirm' with an optional notice line on top. Offers "Save place"
-- unless the pickup already is a favourite.
sendPickupConfirmWith :: Monad m => BotEnv m -> InboundEvent -> FlowContext -> Maybe Text -> Maybe Text -> m ()
sendPickupConfirmWith env ev ctx mNotice mNamedPlace = do
  let s = t env.cfg.translations ctx.language
      to = ev.fromPhone
  save env ev ctx {FlowContext.state = ConfirmingPickup}
  let faMaybe = formatAddress <$> ctx.origin
      areaMaybe = ctx.origin >>= (\o -> o.address.area)
      -- Full address (building/street/area) preferred over the bare area
      -- name — deliberate deviation from engine.ts:1021-1038's area-first
      -- ordering, per product decision to show the precise address instead
      -- of a vague neighborhood name. formatAddress itself already falls
      -- back to "lat, lon" when building/street/area are all empty, so this
      -- is never blank; areaMaybe / "your shared location" remain as
      -- further fallbacks for a Nothing ctx.origin.
      label = firstNonEmpty [mNamedPlace, faMaybe, areaMaybe] "your shared location"
      body = withNotice mNotice $ case mNamedPlace of
        Just np -> s.flexiConfirmSavedPlace np
        Nothing -> s.flexiConfirmPickup label
      canSave = env.cfg.merchant.favouritesEnabled && isNothing mNamedPlace && isJust ctx.personId && maybe False (not . isSavedPlace ctx) ctx.origin
  replyButtons env to body ([btn s.pickupConfirmButton "pickup_confirm", btn s.pickupAdjustButton "pickup_adjust"] <> [btn s.favSaveButton "fav:save" | canSave])

-- | @pickup_confirm@ dispatch (@engine.ts:293-305@): flexi searches immediately,
-- regular asks for a drop. Default for a bare pin: regular-only → regular, else flexi.
handlePickupConfirm :: Monad m => BotEnv m -> InboundEvent -> FlowContext -> m ()
handlePickupConfirm env ev ctx =
  case resolvedRideType env.cfg.merchant ctx of
    Regular -> promptForRegularDrop env ev ctx
    Flexi -> startFlexiSearch env ev ctx

-- | The ride type a pickup is for: the rider's choice, else regular-only → regular,
-- else flexi (a bare pin with no choice made yet).
resolvedRideType :: MerchantCtx -> FlowContext -> RideType
resolvedRideType merchant ctx = case ctx.rideType of
  Just r -> r
  Nothing -> if regularOffered merchant && not (flexiOffered merchant) then Regular else Flexi

-- ---------------------------------------------------------------------------
-- Favourite places (not ported from TS)
-- ---------------------------------------------------------------------------

-- | Pickup and drop states: where the place list can be shown, and so where a
-- tap on one of its rows means something.
isDropState :: FlowState -> Bool
isDropState st = st == AwaitingRegularDrop || st == ConfirmingRegularDrop

-- | A favourite row (@fav:pick:<tag>@): the place becomes the drop when the
-- rider is choosing a drop, else the pickup. A tag that no longer exists
-- (deleted in the app meanwhile) re-asks.
handleFavouritePick :: Monad m => BotEnv m -> InboundEvent -> FlowContext -> Text -> m ()
handleFavouritePick env ev ctx tag = case findFavourite ctx tag of
  Nothing
    | isDropState ctx.state -> promptForRegularDrop env ev ctx
    | otherwise -> promptForPickup env ev ctx
  Just loc
    | isDropState ctx.state -> case (ctx.personId, ctx.origin) of
      (Just _, Just _) -> startRegularSearch env ev ctx {FlowContext.destination = Just (savedToPlace loc)}
      _ -> do
        reply env ev.fromPhone (t env.cfg.translations ctx.language).sessionExpired
        save env ev ctx {FlowContext.state = Idle}
    | otherwise ->
      withPickupChecks env ev ctx (LatLon loc.lat loc.lon) $ \ctx1 _ ->
        continuePickupAt env ev ctx1 (savedToPlace loc) (Just loc.tag)

-- | "Share new location" in the list: today's location request.
shareNewLocation :: Monad m => BotEnv m -> InboundEvent -> FlowContext -> m ()
shareNewLocation env ev ctx = do
  let s = t env.cfg.translations ctx.language
  if isDropState ctx.state
    then do
      save env ev ctx {FlowContext.state = AwaitingRegularDrop}
      locationRequest env ev.fromPhone s.regularDropPrompt
    else do
      save env ev ctx {FlowContext.state = AwaitingPickup}
      locationRequest env ev.fromPhone s.flexiSharePrompt

-- | Where the booking continues after a favourite is saved (or failed to
-- save): the same question the rider was on, with the notice on top.
-- Passed to "WhatsappBot.Flow.Favourites" as its 'Resume' hook.
resumeAfterFavourite :: Monad m => BotEnv m -> InboundEvent -> Text -> FlowContext -> m ()
resumeAfterFavourite env ev notice ctx = case ctx.state of
  ConfirmingPickup -> sendPickupConfirmWith env ev ctx (Just notice) Nothing
  AwaitingPickup -> promptForPickupWith env ev ctx (Just notice)
  st | isDropState st -> promptForRegularDropWith env ev ctx (Just notice)
  _ -> replyWithMenu env ev (menuRow env ev) (notice <> "\n\n")

-- | The location request, then (for a logged-in rider with no favourites yet)
-- a second message with "Add a favourite". WhatsApp's location request can
-- carry no other button, so the offer has to be its own message.
locationRequestWithNudge :: Monad m => BotEnv m -> InboundEvent -> FlowContext -> LanguageStrings -> Text -> m ()
locationRequestWithNudge env ev ctx s body = do
  locationRequest env ev.fromPhone body
  when (isJust ctx.personId) $
    replyButtons env ev.fromPhone s.favAddPrompt [btn s.favAddNew "fav:add"]

-- | Load the rider's saved places when they were never loaded. A login on a
-- bare greeting ('WhatsappBot.Engine.handleIdle') runs no prefetch, and the
-- next 'ensureAuth' returns early for a logged-in rider, so without this a
-- rider with favourites would get the no-favourites prompt. @Just []@ (loaded,
-- none) is kept as is. Only when the merchant has favourites on, so the
-- backend calls of a merchant without them are unchanged.
loadFavouritesIfMissing :: Monad m => BotEnv m -> FlowContext -> m FlowContext
loadFavouritesIfMissing env ctx = case (env.cfg.merchant.favouritesEnabled, ctx.savedLocations, ctx.personId) of
  (True, Nothing, Just pid) -> prefetchSavedLocations env (BotAuth pid) ctx
  _ -> pure ctx

withNotice :: Maybe Text -> Text -> Text
withNotice mNotice body = maybe body (\n -> n <> "\n\n" <> body) mNotice

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
  let s = t env.cfg.translations ctx.language
      to = ev.fromPhone
  case (ctx.personId, ctx.origin) of
    (Just pid, Just origin) -> do
      let auth = BotAuth pid
      save env ev ctx {FlowContext.state = FlexiSearching, FlowContext.cancelRequested = Just False}
      -- Acknowledge at once, with the Cancel button, so the rider isn't left in
      -- silence while quotes are polled. The fare follows as its own message
      -- once a quote exists (see below).
      replyButtons env to s.flexiFinding [btn s.flexiCancelSearch "cancel"]
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
              -- Re-read: a cancel can land during the search above. If the rider
              -- already cancelled, or the conversation has moved on (main menu, a
              -- new pickup), stop here: no fare message and no booking. Booking
              -- anyway would leave a confirmed ride nobody cancels, since
              -- handleCancel ran before the booking existed.
              beforeConfirm <- fromMaybe initialContext <$> getCtx env ev
              if beforeConfirm.cancelRequested == Just True || beforeConfirm.state /= FlexiSearching
                then pure ()
                else do
                  -- Sent only now, once the quote exists: its rate card carries the
                  -- fare policy's base fare and per-km rate.
                  forM_ (flexiFareLine env.cfg.translations chosen ctx.language) (reply env to)
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
                                    { FlowContext.flexiBookingId = Just bookingId,
                                      FlowContext.activeBookingId = Just bookingId,
                                      FlowContext.selectStartedAt = Just (addUTCTime (-120) flowStartedAt),
                                      FlowContext.state = Tracking
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
                                -- Bump only on a delivered card; a failed send releases the
                                -- claim so the tracker retries it. Not bumping on a lost claim
                                -- either: the tracker holding it advances the stage itself.
                                won <- env.registry.claimStage bookingId "assigned"
                                when won $ do
                                  delivered <- sendFlexiDriverCard env ev b
                                  if delivered
                                    then bumpStage env bookingId "assigned"
                                    else env.registry.releaseStage bookingId "assigned"
    _ -> promptForPickup env ev ctx

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
              save env ev fresh {FlowContext.activeBookingId = Just b.bookingId}
              pure (PollFound b)
          _ -> do
            -- Once per search, not every driverPollNotifyEvery attempts: one
            -- reassurance is enough, the rest only add messages.
            when (attempt == env.cfg.driverPollNotifyEvery) $
              reply env (ev.fromPhone) ((t env.cfg.translations lang).flexiStillFinding (fmtInt (((attempt + 1) * env.cfg.driverPollIntervalMs) `div` 1000)))
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
  let s = t env.cfg.translations ctx.language
      to = ev.fromPhone
  save env ev ctx {FlowContext.state = Idle}
  replyButtons env to s.flexiNoAuto [btn s.flexiTryAgain "book"]

-- | Render the shared "auto found" driver card (@engine.ts:1166-1175@) via the
-- shared 'buildDriverCard' — same card the background tracker sends on a restart.
-- Returns whether WhatsApp accepted it, so callers can release the stage claim
-- on a failed send instead of losing the card.
sendFlexiDriverCard :: Monad m => BotEnv m -> InboundEvent -> BotBookingDetails -> m Bool
sendFlexiDriverCard env ev booking = do
  mctx <- getCtx env ev
  let lang = mctx >>= (\c -> c.language)
      card = buildDriverCard env.cfg.translations env.cfg.merchant booking lang
  env.sender.sendButtons (ev.fromPhone) card.bmText card.bmButtons

-- ---------------------------------------------------------------------------
-- Regular one-way flow (engine.ts:735-919)
-- ---------------------------------------------------------------------------

-- | After pickup is confirmed, ask for the drop (@engine.ts:735-748@).
-- A rider with favourites gets the place list instead (they can still type an
-- address, as before).
promptForRegularDrop :: Monad m => BotEnv m -> InboundEvent -> FlowContext -> m ()
promptForRegularDrop env ev ctx = promptForRegularDropWith env ev ctx Nothing

promptForRegularDropWith :: Monad m => BotEnv m -> InboundEvent -> FlowContext -> Maybe Text -> m ()
promptForRegularDropWith env ev ctx0 mNotice = do
  ctx <- loadFavouritesIfMissing env ctx0
  let s = t env.cfg.translations ctx.language
      to = ev.fromPhone
      body = withNotice mNotice s.regularDropPrompt
  save env ev ctx {FlowContext.state = AwaitingRegularDrop}
  if not env.cfg.merchant.favouritesEnabled
    then locationRequest env to body
    else case favourites ctx of
      [] -> locationRequestWithNudge env ev ctx s body
      favs -> sendPlaceList env to s body s.favChooseDrop favs

-- | Capture the drop — a pin (reverse-geocode) or typed address (search →
-- disambiguate) — then price the one-way auto (@engine.ts:752-783@).
handleRegularDrop :: Monad m => BotEnv m -> InboundEvent -> FlowContext -> Text -> m ()
handleRegularDrop env ev ctx input = do
  let s = t env.cfg.translations ctx.language
      to = ev.fromPhone
  case (ctx.personId, ctx.origin) of
    (Just pid, Just _origin) -> do
      let auth = BotAuth pid
      case ev.kind of
        InLocationPin lat lon mName mAddr | input == "__location_pin__" -> do
          edest <- env.backend.reverseGeocode auth (LatLon lat lon)
          let dest0 = placeOrPin lat lon edest
              dest = applyNameOverride dest0 mName mAddr
          startRegularSearch env ev ctx {FlowContext.destination = Just dest}
        _ -> do
          esp <- env.backend.searchPlaces auth input (searchCenterForDest ctx)
          let places = either (const []) identity esp
          if null places
            then reply env to s.noPlacesFound
            else do
              let opts = take 3 (map (\p -> DestinationOption {description = p.description, placeId = p.placeId}) places)
              save env ev ctx {FlowContext.state = ConfirmingRegularDrop}
              replyButtons env to s.regularSelectDrop [btn (T.take 24 o.description) ("regdrop:" <> o.placeId) | o <- opts]
    _ -> do
      reply env to s.sessionExpired
      save env ev ctx {FlowContext.state = Idle}

-- | Rider picked a searched drop option (or typed a new address) (@engine.ts:786-803@).
handleConfirmingRegularDrop :: Monad m => BotEnv m -> InboundEvent -> FlowContext -> Text -> m ()
handleConfirmingRegularDrop env ev ctx input = do
  let s = t env.cfg.translations ctx.language
      to = ev.fromPhone
  if "regdrop:" `T.isPrefixOf` input
    then case ctx.personId of
      Nothing -> reply env to s.sessionExpired
      Just pid -> do
        let auth = BotAuth pid
        ep <- env.backend.getPlaceDetails auth (T.drop 8 input)
        case ep of
          Left _ -> reply env to s.somethingWentWrong
          Right place -> startRegularSearch env ev ctx {FlowContext.destination = Just place}
    else handleRegularDrop env ev ctx input

-- | Price the one-way auto (ONE_WAY search → estimates → pick auto) then show the
-- fare confirmation (@engine.ts:807-842@).
startRegularSearch :: Monad m => BotEnv m -> InboundEvent -> FlowContext -> m ()
startRegularSearch env ev ctx = do
  let s = t env.cfg.translations ctx.language
      to = ev.fromPhone
  case (ctx.personId, ctx.origin, ctx.destination) of
    (Just pid, Just origin, Just destination) -> do
      let auth = BotAuth pid
      -- No "Getting your fare..." message: the fare confirmation follows within
      -- seconds. A message sent while searching still gets that reply (stateSwitch).
      save env ev ctx {FlowContext.state = RegularSearching}
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
              sendRegularFareConfirm env ev ctx {FlowContext.regularEstimateId = Just autoEst.estimateId, FlowContext.regularFare = Just autoEst.estimatedFare}
    _ -> do
      reply env to s.sessionExpired
      save env ev ctx {FlowContext.state = Idle}

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
  let s = t env.cfg.translations ctx.language
      to = ev.fromPhone
  save env ev ctx {FlowContext.state = ConfirmingRegularFare}
  -- Full addresses (building, street, area) for both ends, not just the drop's area.
  let dropAddr = maybe "your destination" formatAddress ctx.destination
      pickupAddr = maybe "your pickup" formatAddress ctx.origin
  replyButtons
    env
    to
    (s.regularFareConfirm (fmtNum (fromMaybe 0 ctx.regularFare)) dropAddr pickupAddr)
    -- Change pickup reuses pickup_adjust: re-ask the pickup, then the drop again.
    [btn s.regularConfirmButton "regular_book", btn s.regularChangeDropButton "regular_change_drop", btn s.regularChangePickupButton "pickup_adjust"]

-- | Book the one-way auto: select the estimate, poll for a driver, show the card,
-- register with the tracker (@engine.ts:861-919@).
confirmRegularBooking :: Monad m => BotEnv m -> InboundEvent -> FlowContext -> m ()
confirmRegularBooking env ev ctx = do
  let s = t env.cfg.translations ctx.language
      to = ev.fromPhone
  case (ctx.personId, ctx.regularEstimateId) of
    (Just pid, Just estimateId) -> do
      let auth = BotAuth pid
      save env ev ctx {FlowContext.cancelRequested = Just False}
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
              save env ev afterSelect {FlowContext.state = Tracking, FlowContext.selectStartedAt = Just selectCalledAt}
              outcome <- pollRegularBooking env ev auth selectCalledAt ctx.language 0
              case outcome of
                PollAborted -> pure ()
                PollNotFound -> flexiNoAuto env ev ctx
                PollFound b -> do
                  registerRide' env ev b.bookingId ctx "confirmed"
                  let hasDriver = isJust b.driverName || isJust b.rideOtp
                  won <- if hasDriver then env.registry.claimStage b.bookingId "assigned" else pure False
                  delivered <- sendFlexiDriverCard env ev b
                  -- Same rule as the flexi path: advance only on a delivered card,
                  -- otherwise release so the tracker retries.
                  when won $
                    if delivered
                      then bumpStage env b.bookingId "assigned"
                      else env.registry.releaseStage b.bookingId "assigned"
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
            save env ev fresh {FlowContext.activeBookingId = Just b.bookingId}
            pure (PollFound b)
          [] -> do
            -- Once per search, not every driverPollNotifyEvery attempts: one
            -- reassurance is enough, the rest only add messages.
            when (attempt == env.cfg.driverPollNotifyEvery) $
              reply env (ev.fromPhone) ((t env.cfg.translations lang).flexiStillFinding (fmtInt (((attempt + 1) * env.cfg.driverPollIntervalMs) `div` 1000)))
            env.clock.sleepMs env.cfg.driverPollIntervalMs
            pollRegularBooking env ev auth createdAfter lang (attempt + 1)

-- ---------------------------------------------------------------------------
-- Flexi end-OTP (engine.ts:325-359)
-- ---------------------------------------------------------------------------

-- | @flexi_end_otp:<id>@ (@engine.ts:325-359@): reveal the rental end OTP on
-- demand, with a path-injection guard + registry ownership check (IDOR defense).
handleFlexiEndOtp :: Monad m => BotEnv m -> InboundEvent -> FlowContext -> Text -> m ()
handleFlexiEndOtp env ev ctx bid = do
  let s = t env.cfg.translations ctx.language
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
                else -- A ride type with no end OTP has no button to answer: one tapped from
                -- before this check existed would only resend the copied start OTP,
                -- so stay silent.
                when b.endOtpRequired $ case b.endOtp of
                  Just eo -> replyButtons env to (s.flexiEndOtpShare eo) [btn s.flexiEndRideButton ("flexi_end_otp:" <> bid)]
                  Nothing -> reply env to s.flexiEndOtpNotReady
            _ -> replyButtons env to s.flexiEndOtpFetchError [btn s.flexiEndRideButton ("flexi_end_otp:" <> bid)]
        _ -> reply env to s.flexiRideAlreadyEnded

-- ---------------------------------------------------------------------------
-- The menu row + merchant capabilities (engine.ts:1530-1546)
-- ---------------------------------------------------------------------------

-- | The context-aware menu row (@engine.ts:1530-1546@, changed): [Track?] · the
-- ride types shown directly · More (Track shown only when a ride is live). The
-- ride types sit in the welcome itself so booking needs no extra "Book" tap;
-- Language and any ride type that doesn't fit ('splitRideTypes') move under More.
-- With Track that is up to 4 buttons, which the sender turns into a list.
menuRow :: Monad m => BotEnv m -> InboundEvent -> LanguageStrings -> m [OutButton]
menuRow env ev s = do
  let merchant = env.cfg.merchant
      uk = mkUserKey merchant ev
  has <- env.registry.hasActiveRide uk
  let trackBtn = [btn s.trackRide "status" | has]
      bookBtns = case merchant.rideTypesOrder of
        [Regular] -> [btn s.bookARide "ride_type:regular"]
        rts -> map (rideTypeBtn s) (fst (splitRideTypes merchant.maxDirectButtons rts))
  pure (trackBtn <> bookBtns <> [btn s.moreButton "more"])

flexiOffered :: MerchantCtx -> Bool
flexiOffered merchant = Flexi `elem` merchant.rideTypesOrder

regularOffered :: MerchantCtx -> Bool
regularOffered merchant = Regular `elem` merchant.rideTypesOrder

-- ---------------------------------------------------------------------------
-- Ride-type chooser button layout: a merchant's rideTypesOrder -> the
-- WhatsApp buttons for the initial prompt + the "More" reveal.
--
-- Default layout rule (design discussion, not ported from TS — this is new):
--   total <= 1 -> show it directly, no More (nothing to hide)
--   total >= 2 -> show min(total - 1, 2) directly, in priority order;
--                 everything else goes behind "More"
-- The "-1" reserves a slot for the More button itself whenever there IS a
-- hidden item; capped at 2 so direct+More never exceeds WhatsApp's real
-- 3-button-per-message limit. This is the ONLY rule used when a merchant's
-- maxDirectButtons is Nothing — unchanged from before maxDirectButtons existed.
--
-- Just n (explicit per-merchant override, e.g. "always nudge to priority
-- option even when both would fit"): cap = clamp(n, 1, 2). If everything
-- fits within cap, show it all directly with NO "More" (no reservation) —
-- otherwise show `cap` directly and hide the rest. This is a genuinely
-- different rule from the Nothing case (no automatic "-1" reservation), so
-- Just 2 at total=2 shows both with no More, which Nothing never does.
--
-- Wire ids for direct buttons are UNCHANGED ("ride_type:flexi"/
-- "ride_type:regular" — a byte-identical contract, see this package's own
-- CLAUDE.md), so handleRideType needs no changes.
-- ---------------------------------------------------------------------------

-- | One direct button for a single ride type.
rideTypeBtn :: LanguageStrings -> RideType -> OutButton
rideTypeBtn s Flexi = btn s.rideTypeFlexi "ride_type:flexi"
rideTypeBtn s Regular = btn s.rideTypeRegular "ride_type:regular"

-- | Split a merchant's offered ride types into (shown directly, behind More),
-- per the layout rule above. 'Nothing' keeps the original default rule
-- byte-for-byte; 'Just n' is the explicit per-merchant override.
splitRideTypes :: Maybe Int -> [RideType] -> ([RideType], [RideType])
splitRideTypes Nothing rts
  | length rts <= 1 = (rts, [])
  | otherwise = splitAt (min (length rts - 1) 2) rts
splitRideTypes (Just n) rts
  | length rts <= cap = (rts, [])
  | otherwise = splitAt cap rts
  where
    cap = max 1 (min n 2)

-- | The ride-type chooser's initial set of buttons for a merchant's full
-- offered list (used by 'sendRideTypePrompt').
rideTypeButtons :: LanguageStrings -> Maybe Int -> [RideType] -> [OutButton]
rideTypeButtons s maxDirect rts =
  let (direct, hidden) = splitRideTypes maxDirect rts
   in map (rideTypeBtn s) direct <> [btn s.moreRideTypes "more_ride_types" | not (null hidden)]

-- | The buttons "More" reveals: the hidden remainder from 'rideTypeButtons'.
-- Recomputed fresh from the merchant's config on each tap — nothing about
-- which types were hidden is persisted between the initial prompt and the
-- More tap.
hiddenRideTypeButtons :: LanguageStrings -> Maybe Int -> [RideType] -> [OutButton]
hiddenRideTypeButtons s maxDirect rts = map (rideTypeBtn s) (snd (splitRideTypes maxDirect rts))

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
  Just nm | isNothing p.address.area -> p {BotPlace.address = (p.address) {BotAddress.area = Just nm}}
  _ -> p

-- | The tariff display line (@engine.ts:571-579@), from the quote's fare-policy
-- rates; Nothing when the quote carries no rate card.
flexiFareLine :: Map.Map SupportedLanguage LanguageStrings -> BotQuote -> Maybe SupportedLanguage -> Maybe Text
flexiFareLine translations quote lang = case (quote.baseFare, quote.perKmFare) of
  (Just base, Just perKm) -> Just ((t translations lang).flexiFareRate (fmtNum base) (fmtNum perKm))
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
