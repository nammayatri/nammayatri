-- | The engine's runtime environment: all the effect handles bundled with the
-- static config, plus the conversation primitives every flow is built out of —
-- session read/write, the outbound reply verbs, and the identity ladder
-- ('ensureAuth'). The pure engine takes a @BotEnv m@ and never reaches for any
-- effect outside it, so the golden suite injects a mock env and rider-app
-- injects a 'Flow' env with the same engine code.
--
-- The intent is that a second WhatsApp flow imports this module and nothing else
-- of the engine. This module must NEVER import @WhatsappBot.Engine@, or any
-- @WhatsappBot.Flow.*@ module (the dependency edge runs one way only).
--
-- Everything here IS flow-agnostic. The one construct that is not — the menu
-- row, which hardcodes the booking flow's @"ride_type:flexi"@ /
-- @"ride_type:regular"@ wire strings and copy — was moved out to
-- @WhatsappBot.Flow.Booking@ (along with 'flexiOffered' / 'regularOffered', its
-- merchant-capability predicates). 'replyWithMenu' stays here and takes the row
-- as a PARAMETER, the same hook technique 'ensureAuth' uses for its post-auth
-- prefetch, so a second flow reuses 'replyWithMenu' with its OWN row instead of
-- inheriting the booking flow's buttons.
module WhatsappBot.Env
  ( BotConfig (..),
    BotEnv (..),

    -- * Conversation keys
    scopedSessionId,
    mkUserKey,

    -- * Session context
    save,
    getCtx,
    resetContext,

    -- * Outbound verbs
    reply,
    replyButtons,
    locationRequest,
    btn,
    replyWithMenu,

    -- * Inbound
    rawInput,
    extractPhone,

    -- * Auth
    ensureAuth,

    -- * Small pure helpers
    firstNonEmpty,
  )
where

import Control.Applicative ((<|>))
import Data.Char (isDigit)
import qualified Data.Text as T
import Kernel.Prelude
import WhatsappBot.Handles
import WhatsappBot.I18n (LanguageStrings, t)
-- Instances only: LanguageStrings / LanguageInfo are dot-accessed via RDP
-- (getField), so their selectors are never referenced by name, but the HasField
-- instances are needed. (Naming them would trip -Wunused-imports under -Werror.)
import WhatsappBot.I18n.Types ()
import WhatsappBot.Types

-- | Static per-conversation config: the allowlist, the resolved merchant, and
-- the poll-loop constants (TS parity, @engine.ts@/@config.ts@). Held separate
-- from the handles so tests can vary it without touching the effect layer.
data BotConfig = BotConfig
  { -- | Normalized 10-digit allowlist; empty = open to all (@config.ts:112-118@;
    -- default @["9361176218"]@).
    allowedPhones :: [Text],
    -- | The merchant resolved for this session (by phone_number_id upstream).
    merchant :: MerchantCtx,
    -- | Flexi quote poll: attempts × interval (@engine.ts:1074-1078@; 10 × 2000ms).
    flexiQuotePollAttempts :: Int,
    flexiQuotePollIntervalMs :: Int,
    -- | Regular estimate poll (@engine.ts:825-829@; 6 × 2000ms).
    regularEstimatePollAttempts :: Int,
    regularEstimatePollIntervalMs :: Int,
    -- | Driver-assignment poll (@engine.ts:890-906,1123-1151@; 90 × 2000ms,
    -- progress notify every 15).
    driverPollAttempts :: Int,
    driverPollIntervalMs :: Int,
    driverPollNotifyEvery :: Int
  }

-- INVARIANT — do not weaken. The engine is polymorphic in @m@ with @Monad m@ as
-- its ONLY constraint. Every effect (backend calls, outbound sends, session and
-- person stores, the ride registry, time, delay) is reached through a handle in
-- this record, never directly.
--
-- This is what lets the golden replay suite run the REAL engine in @IO@ over
-- @IORef@s while production runs the same code in rider-app's @Flow@. Adding
-- @MonadIO@, @MonadThrow@, @MonadReader@, or any other constraint anywhere in
-- the engine destroys the oracle. If you think you need one, you need a new
-- field in a handle instead.

-- | Everything the engine needs, in one record. @m@ is 'IO' (over mock IORefs)
-- in the golden suite and rider-app's @Flow@ in production.
data BotEnv m = BotEnv
  { backend :: BackendHandle m,
    sender :: WaSender m,
    sessions :: SessionStore m,
    persons :: PersonStore m,
    registry :: RideRegistry m,
    clock :: Clock m,
    cfg :: BotConfig
  }

-- ---------------------------------------------------------------------------
-- Conversation keys (app.ts:43-46, engine.ts:1617-1620)
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

-- ---------------------------------------------------------------------------
-- Auth / silent onboarding (engine.ts:615-682; D2/D3)
-- ---------------------------------------------------------------------------

-- | @ensureAuth@ (@engine.ts:615-682@), 3-layer lookup ORDER preserved so the
-- golden backend-call order replays: (1) ctx.personId present → ok; (2) durable
-- PersonStore hit → adopt + run the hook; (3) derive phone → @authenticate@
-- (prod: find-or-create, no OTP — D2/D3) → run the hook → persist. Returns
-- @Just ctx'@ (ok, ctx' saved) or @Nothing@ (failed; a message was already sent,
-- state reset to IDLE).
--
-- The hook is where a flow does its post-auth prefetch. Booking passes
-- @prefetchSavedLocations@ (preserving @authenticate → getSavedLocations@,
-- which the goldens assert as an ORDERED sequence); a flow with no prefetch
-- passes @\\_ c -> pure c@. The hook MUST fire at the same two points the
-- inline prefetch fired (engine.ts:276, :296) or the golden order breaks.
--
-- HOOK CONTRACT: the hook is handed an ALREADY-AUTHENTICATED context — layer
-- (3) applies @personId@ from the fresh 'BotAuth' BEFORE calling it, so both
-- call sites look identical to the hook — and it MUST preserve @personId@.
-- @setPerson@ takes @personId@ from the pre-hook 'BotAuth' value, not from the
-- hook's returned context, so the durable PersonStore record is immune to a
-- misbehaving hook; the one field the hook DOES control there is @language@,
-- which does flow from the hook's returned context into @setPerson@. @save@ is
-- the exposure: it persists the hook's returned context verbatim as the
-- session, so a hook that dropped or overwrote @personId@ would corrupt only
-- the SESSION, not the person record — the practical failure mode is that the
-- next turn sees no @personId@ and simply re-runs the auth ladder. No fixture
-- would catch that either way: the golden oracle records only 'BackendHandle'
-- calls and outbound sends, and never sees session or person-store writes.
ensureAuth ::
  Monad m =>
  BotEnv m ->
  InboundEvent ->
  (BotAuth -> FlowContext -> m FlowContext) ->
  FlowContext ->
  m (Maybe FlowContext)
ensureAuth env ev hook ctx = do
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
          ctx2 <- hook auth ctx1
          save env ev ctx2
          pure (Just ctx2)
        Nothing -> case extractPhone ev of
          Nothing -> do
            reply env to s.sessionExpired
            save env ev ctx {state = Idle}
            pure Nothing
          Just phone -> do
            eauth <- env.backend.authenticate phone
            case eauth of
              Left err -> do
                reply env to (s.setupFailed err.botErrorMessage)
                save env ev ctx {state = Idle}
                pure Nothing
              Right auth -> do
                ctx2 <- hook auth (ctx {personId = Just auth.personId} :: FlowContext)
                env.persons.setPerson uk StoredPerson {personId = auth.personId, language = ctx2.language}
                save env ev ctx2
                pure (Just ctx2)

-- ---------------------------------------------------------------------------
-- Menu / context helpers
-- ---------------------------------------------------------------------------

-- | @replyWithMenu@ (@engine.ts:1357-1364@): a prefix + "what to do" + menu row.
--
-- @mkRow@ is the caller's menu row. It is a PARAMETER rather than a call to a
-- fixed @menuRow@ because the row is flow-specific: the booking flow's row
-- carries @"ride_type:flexi"@ / @"ride_type:regular"@ button ids
-- (@Flow.Booking.menuRow@), and a second flow needs its own. It is a HOOK
-- (@LanguageStrings -> m [OutButton]@) rather than a plain @[OutButton]@
-- because the row's copy depends on the language read from the FRESH context
-- below — the caller cannot build it before this function reads that context.
replyWithMenu :: Monad m => BotEnv m -> InboundEvent -> (LanguageStrings -> m [OutButton]) -> Text -> m ()
replyWithMenu env ev mkRow prefix = do
  mctx <- getCtx env ev
  let s = t (mctx >>= (\c -> c.language))
  row <- mkRow s
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

-- | Extract a normalized 10-digit phone from the channel (@engine.ts:1647-1654@):
-- strip non-digits, drop a leading @91@ when longer than 10, require exactly 10.
extractPhone :: InboundEvent -> Maybe Text
extractPhone ev =
  let digits = T.filter isDigit ev.fromPhone
      d = if "91" `T.isPrefixOf` digits && T.length digits > 10 then T.drop 2 digits else digits
   in if T.length d == 10 then Just d else Nothing
