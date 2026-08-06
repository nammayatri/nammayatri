{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Inbound Meta WhatsApp Cloud API webhook handler. GET = verification
-- handshake; POST = ack-fast delivery (verify signature → decode → dedupe →
-- fork). Merchant is resolved from @phone_number_id@ against the app-env map
-- (@metaWebhookMerchants@); the per-message engine dispatch is stubbed here and
-- wired to the prod handles + @WhatsappBot.Engine.handleMessage@ in Step 4.
module Domain.Action.UI.MetaWhatsappWebhook
  ( getWebhookChallenge,
    postWebhook,
  )
where

import qualified Data.Text as T
import Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Meta as Meta
import Kernel.External.Meta.Webhook (RawByteString)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Error
import Kernel.Utils.Common
import qualified WhatsappBot.Adapter.Env as Env
import WhatsappBot.Inbound (parseInbound)
import WhatsappBot.Types (InboundEvent)

-- | GET verification handshake (@whatsapp/webhook@). Fail-closed if the verify
-- token is not configured (empty), else echo @hub.challenge@ verbatim iff
-- @hub.mode == "subscribe"@ and the token matches (constant-time, in the kernel
-- helper). A non-match is answered with a 4xx, which is Meta-compliant.
getWebhookChallenge :: Maybe Text -> Maybe Text -> Maybe Text -> Flow Text
getWebhookChallenge mMode mToken mChallenge = withLogTag "MetaWebhook" $ do
  cfgToken <- asks (.metaVerifyToken)
  when (T.null cfgToken) $ throwError (InvalidRequest "META_VERIFY_TOKEN_NOT_CONFIGURED")
  case Meta.metaVerifyChallenge cfgToken mMode mToken mChallenge of
    Just c -> pure c
    Nothing -> throwError (InvalidRequest "META_WEBHOOK_VERIFY_FAILED")

-- | POST inbound delivery. Handler wall-time = verify + decode + Redis only
-- (heavy work is forked), so Meta gets its fast 200 (it retries for up to 7
-- days otherwise). Signature/secret failures reject; everything else acks.
postWebhook :: Maybe Text -> RawByteString -> Flow APISuccess
postWebhook mbSig rawBody = withLogTag "MetaWebhook" $ do
  enabled <- asks (.metaBotEnabled)
  -- Kill-switch = DROP, not pause: a disabled webhook still acks 200 so Meta stops
  -- retrying — messages arriving while disabled are discarded, NOT queued. Enable
  -- metaBotEnabled before pointing Meta's callback here.
  if not enabled
    then pure Success
    else do
      appSecret <- asks (.metaAppSecret)
      -- Fail-closed: an empty appSecret makes the HMAC forgeable (empty key), so
      -- never verify against it — reject as misconfigured instead.
      when (T.null appSecret) $ throwError (InvalidRequest "META_APP_SECRET_NOT_CONFIGURED")
      Meta.verifyMetaSignature appSecret mbSig rawBody -- throws INVALID_META_SIGNATURE on mismatch
      case Meta.decodeWebhookEnvelope rawBody of
        Left err -> do
          logError $ "Meta webhook decode failed (acking to stop 7d retries): " <> T.pack err
          pure Success
        Right envelope -> do
          logChangeEvents envelope -- D4: statuses/errors/unknown explicitly logged, never silently dropped
          let allEvents = parseInbound envelope
              -- Match TS parseIncoming (whatsapp.ts:58-62): first inbound message only.
              events = take 1 allEvents
          -- Keep the "never silently dropped" invariant honest: warn when a batch
          -- tail is dropped for TS parity (Meta can batch after downtime).
          when (length allEvents > 1) $
            logWarning $ "Meta webhook: envelope carried " <> show (length allEvents) <> " messages; processing the first only (TS parity), dropping " <> show (length allEvents - 1)
          merchants <- asks (.metaWebhookMerchants)
          forM_ events $ \(phoneNumberId, ev) ->
            case find (\m -> m.phoneNumberId == phoneNumberId) merchants of
              Nothing ->
                logWarning $ "Meta webhook: no merchant mapped for phoneNumberId=" <> phoneNumberId
              Just merchant -> do
                -- Dedupe on messages[].id (Meta redelivers): SET NX ⇒ first-seen only.
                -- At-most-once BY DESIGN: the id is marked seen BEFORE the forked work
                -- runs, so a mid-process crash consumes the message (no redelivery after
                -- our 200). Fine for a chat bot.
                fresh <- Redis.setNxExpire (dedupKey ev.messageId) dedupTtlSec True
                -- setNxExpire == False is ambiguous (real duplicate vs a swallowed Redis
                -- error). Confirm with a read so a Redis blip doesn't silently drop the
                -- user's message behind the 200 ack: only a present key is a real dup.
                isDup <-
                  if fresh
                    then pure False
                    else isJust <$> (Redis.get (dedupKey ev.messageId) :: Flow (Maybe Bool))
                if isDup
                  then logInfo $ "Meta webhook: duplicate message " <> ev.messageId <> " — skipped"
                  else fork "whatsapp bot inbound" $ processInbound merchant ev
          pure Success

-- | D4: surface every non-message change (statuses / errors / unknown) so they
-- are logged, never silently dropped — including any that co-occur with a
-- message in the same envelope. Message changes are handled downstream by
-- 'parseInbound' + the dispatch loop. Statuses stay log-and-drop this phase (L10).
logChangeEvents :: Meta.MetaWebhookEnvelope -> Flow ()
logChangeEvents envelope =
  forM_ [Meta.classifyChangeValue change.value | entry <- envelope.entry, change <- entry.changes] $ \case
    Meta.EventStatuses sts -> logInfo $ "Meta webhook: " <> show (length sts) <> " status update(s) — logged, no action (L10)"
    Meta.EventErrors errs -> logWarning $ "Meta webhook: error change: " <> show errs
    Meta.EventUnknown -> logInfo "Meta webhook: unknown change shape — ignored"
    Meta.EventMessages {} -> pure () -- handled via parseInbound + dispatch

dedupKey :: Text -> Text
dedupKey msgId = "wab:dedup:" <> msgId

-- | Cover Meta's full redelivery window (docs: up to 7 days) so a late
-- redelivery can't slip past the dedupe and get double-processed.
dedupTtlSec :: Redis.ExpirationTime
dedupTtlSec = 604800 -- 7 days

-- | Dispatch one inbound message to the golden-tested engine with the prod handles
-- (DB merchant/city resolution, @MetaBotCfg@→@MerchantCtx@, @resolveSession@,
-- @WhatsappBot.Engine.handleMessage@), via 'Env.dispatchInbound' which wraps the
-- run in a reply-on-error backstop. This is the fork body — all heavy work (auth,
-- search, poll loops) runs here, off the webhook's fast 200 ack.
processInbound :: MetaWebhookMerchant -> InboundEvent -> Flow ()
processInbound merchant ev = do
  logInfo $
    "Meta webhook dispatch: merchant="
      <> merchant.merchantShortId
      <> " city="
      <> merchant.city
      <> " from="
      <> maskPhone ev.fromPhone
      <> " messageId="
      <> ev.messageId
  Env.dispatchInbound merchant ev

-- | Mask a phone number for logs. This repo treats mobile numbers as sensitive
-- (DbHash / EncryptedField / the redacting Show on MetaCfg), so never log them raw.
maskPhone :: Text -> Text
maskPhone p = "***" <> T.takeEnd 4 p
