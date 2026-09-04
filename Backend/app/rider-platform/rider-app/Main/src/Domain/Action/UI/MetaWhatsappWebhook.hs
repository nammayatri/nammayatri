{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Inbound Meta WhatsApp Cloud API webhook handler. GET = verification
-- handshake; POST = ack-fast delivery (peek phoneNumberId → look up
-- MetaWebhookConfig → decrypt → verify signature → decode → dedupe → fork).
-- Per-phone-number App Secret/Verify Token/Access Token/merchant routing now
-- live in the @meta_config@ DB table (Storage.CachedQueries.MetaWebhookConfig),
-- not Dhall — multiple Meta Apps/phone numbers can each have their own
-- secret. See the peek-before-verify precedent in
-- provider-platform/.../Tools/SignatureAuth.hs for the general shape.
module Domain.Action.UI.MetaWhatsappWebhook
  ( getWebhookChallenge,
    postWebhook,
  )
where

import Data.Aeson ((.:))
import qualified Data.Aeson as A
import Data.ByteArray (constEq)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Domain.Types.MetaWebhookConfig (MetaWebhookConfig)
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.Meta as Meta
import Kernel.External.Meta.Webhook (RawByteString)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Utils.Common
import qualified Storage.CachedQueries.MetaWebhookConfig as CQMWC
import qualified Storage.Queries.MetaWebhookConfig as QMWC
import Tools.Error (MetaWebhookError (MetaAppSecretNotConfigured, MetaWebhookSignatureInvalid, MetaWebhookVerifyFailed))
import qualified WhatsappBot.Adapter.Env as Env
import WhatsappBot.Inbound (parseInbound)
import WhatsappBot.Types (InboundEvent)

-- | Minimal, tolerant peek at just enough of the envelope to resolve which
-- MetaWebhookConfig row owns this request — decoded from the SAME raw bytes
-- 'Meta.verifyMetaSignature' checks against, further down. Never throws: a
-- malformed/foreign-shaped body decodes to Nothing, which the caller acks as
-- a 200 (never a 500) — Meta's own pings and unrelated bodies must not crash
-- this. Only the first entry/first change is inspected, mirroring
-- 'parseInbound's "first message only" policy.
newtype PhonePeek = PhonePeek {phoneNumberId :: Text}

instance A.FromJSON PhonePeek where
  parseJSON = A.withObject "PhonePeek" $ \o -> do
    (entry0 : _) <- o .: "entry"
    (change0 : _) <- entry0 .: "changes"
    value <- change0 .: "value"
    metadata <- value .: "metadata"
    PhonePeek <$> metadata .: "phone_number_id"

peekPhoneNumberId :: RawByteString -> Maybe Text
peekPhoneNumberId rawBody =
  (.phoneNumberId) <$> (A.decodeStrict (LBS.toStrict (Meta.getRawByteString rawBody)) :: Maybe PhonePeek)

-- | GET verification handshake (@whatsapp/webhook@). Meta's GET carries no
-- phone_number_id (it's tied to the App-level webhook subscription, not any
-- one phone number), so unlike the POST path there's nothing to look up BY —
-- accept if the presented token matches ANY currently-enabled row's
-- verifyToken. Not a hot path (fires only when someone (re-)saves the
-- webhook subscription in a Meta App dashboard), so a full-table scan with
-- no caching is the right amount of simple.
getWebhookChallenge :: Maybe Text -> Maybe Text -> Maybe Text -> Flow Text
getWebhookChallenge mMode mToken mChallenge = withLogTag "MetaWebhook" $
  case (mMode, mToken, mChallenge) of
    (Just "subscribe", Just token, Just challenge) -> do
      allConfigs <- QMWC.findAll
      let configs = filter (.enabled) allConfigs
      matched <- anyM (tokenMatches token) configs
      if matched
        then pure challenge
        else throwError MetaWebhookVerifyFailed
    _ -> throwError MetaWebhookVerifyFailed
  where
    -- anyM comes from EulerHS.Prelude (Universum) — don't shadow it with a
    -- hand-rolled version.
    tokenMatches presented cfg = do
      cfgToken <- decrypt cfg.verifyToken
      pure $ constEq (encodeUtf8 presented :: BS.ByteString) (encodeUtf8 cfgToken :: BS.ByteString)

-- | POST inbound delivery. Handler wall-time = peek + lookup + decrypt +
-- verify + decode + Redis only (heavy work is forked), so Meta gets its fast
-- 200 (it retries for up to 7 days otherwise). Signature/secret failures
-- reject; everything else acks.
postWebhook :: Maybe Text -> RawByteString -> Flow APISuccess
postWebhook mbSig rawBody = withLogTag "MetaWebhook" $
  case peekPhoneNumberId rawBody of
    Nothing -> do
      logWarning "Meta webhook: could not peek phone_number_id from body — acking, no retry"
      pure Success
    Just phoneNumberId ->
      CQMWC.findByPhoneNumberId phoneNumberId >>= \case
        Nothing -> do
          logWarning $ "Meta webhook: no MetaWebhookConfig row for phoneNumberId=" <> phoneNumberId
          pure Success
        Just cfg
          | not cfg.enabled -> pure Success -- per-row kill-switch: DROP, not pause — matches "ack, don't retry" semantics
          | otherwise -> handleVerifiedWebhook cfg mbSig rawBody

handleVerifiedWebhook :: MetaWebhookConfig -> Maybe Text -> RawByteString -> Flow APISuccess
handleVerifiedWebhook cfg mbSig rawBody = do
  appSecret <- decrypt cfg.appSecret
  -- Fail-closed: an empty appSecret makes the HMAC forgeable (empty key), so
  -- never verify against it — reject as misconfigured instead.
  when (T.null appSecret) $ throwError MetaAppSecretNotConfigured
  -- Pure check (not Meta.verifyMetaSignature, which throws the same generic
  -- InvalidRequest "INVALID_META_SIGNATURE" this whole file moved away from)
  -- so a mismatch surfaces as our own typed MetaWebhookSignatureInvalid
  -- instead. Verified against the SAME rawBody the peek read.
  unless (Meta.verifyMetaSignaturePure appSecret mbSig (Meta.getRawByteString rawBody)) $
    throwError MetaWebhookSignatureInvalid
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
      forM_ events $ \(evPhoneNumberId, ev) ->
        if evPhoneNumberId /= cfg.phoneNumberId
          then -- Drift guard: the peeked/looked-up id and the decoded envelope's id
          -- disagree (shouldn't happen — same JSON, same field — but if the
          -- peek and the full decoder ever diverge, fail closed on THIS
          -- message rather than dispatch under the wrong tenant's secret).
            logWarning $ "Meta webhook: envelope phoneNumberId=" <> evPhoneNumberId <> " != resolved=" <> cfg.phoneNumberId
          else do
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
              else fork "whatsapp bot inbound" $ processInbound cfg ev
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
processInbound :: MetaWebhookConfig -> InboundEvent -> Flow ()
processInbound cfg ev = do
  logInfo $
    "Meta webhook dispatch: merchant="
      <> cfg.botConfig.merchantLabel
      <> " merchantId="
      <> cfg.merchantId.getId
      <> " from="
      <> maskPhone ev.fromPhone
      <> " messageId="
      <> ev.messageId
  Env.dispatchInbound cfg ev

-- | Mask a phone number for logs. This repo treats mobile numbers as sensitive
-- (DbHash / EncryptedField / the redacting Show on MetaCfg), so never log them raw.
maskPhone :: Text -> Text
maskPhone p = "***" <> T.takeEnd 4 p
