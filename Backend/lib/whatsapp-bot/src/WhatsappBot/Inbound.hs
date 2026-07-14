-- | Parse a decoded Meta webhook envelope into the port-local 'InboundEvent's
-- the engine consumes — the ONE place whatsapp-bot touches
-- 'Kernel.External.Meta'. Port of the parse in @connectors/whatsapp.ts:53-113@.
-- Both the golden harness and the rider-app webhook handler use this, so the
-- inbound-parse logic lives exactly once.
--
-- Per message: @text@ -> 'InText' (body, or "" if absent, matching TS
-- @text?.body || ''@); @interactive@ -> 'InButtonTap' (button_reply/list_reply
-- id, or "" for an unknown reply, matching @button_reply?.id || list_reply?.id
-- || ''@); @location@ -> 'InLocationPin'; any other type is dropped (TS returns
-- null). @classifyChangeValue@ selects message-bearing changes; status/error
-- changes yield no events here.
module WhatsappBot.Inbound
  ( ParsedInbound,
    parseInbound,
  )
where

import qualified Kernel.External.Meta as Meta
import Kernel.Prelude
import WhatsappBot.Types (InboundEvent (..), InboundKind (..))

-- | @(phoneNumberId, event)@ — the merchant is resolved from the phoneNumberId
-- by the caller (harness / webhook), never here.
type ParsedInbound = (Text, InboundEvent)

-- | Every inbound message across all entries/changes, in envelope order. A
-- single golden-fixture envelope carries one message, so callers that mirror TS
-- @parseIncoming@ (first message only) take the head.
parseInbound :: Meta.MetaWebhookEnvelope -> [ParsedInbound]
parseInbound env =
  [ (meta.phoneNumberId, ev)
    | entry <- env.entry,
      change <- entry.changes,
      Meta.EventMessages meta contacts msgs <- [Meta.classifyChangeValue change.value],
      msg <- msgs,
      Just ev <- [toEvent contacts msg]
  ]

toEvent :: [Meta.MetaContact] -> Meta.MetaInboundMessage -> Maybe InboundEvent
toEvent contacts msg = do
  k <- parseKind msg
  let contact = listToMaybe contacts
  pure
    InboundEvent
      { fromPhone = msg.from,
        waId = (\c -> c.waId) <$> contact,
        profileName = contact >>= (\c -> c.profile) >>= (\p -> p.name),
        messageId = msg.id,
        kind = k
      }

parseKind :: Meta.MetaInboundMessage -> Maybe InboundKind
parseKind msg = case msg.type_ of
  "text" -> Just (InText (maybe "" (\t -> t.body) msg.text))
  "interactive" ->
    Just . InButtonTap $ case msg.interactive of
      Just (Meta.MetaButtonReply d) -> d.id
      Just (Meta.MetaListReply d) -> d.id
      _ -> "" -- MetaUnknownReply / absent: TS optional-chains to ''
  "location" ->
    (\loc -> InLocationPin loc.latitude loc.longitude loc.name loc.address) <$> msg.location
  _ -> Nothing -- other message types are dropped (TS returns null)
