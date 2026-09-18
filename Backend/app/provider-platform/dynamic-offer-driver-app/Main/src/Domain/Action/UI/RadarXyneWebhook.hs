-- | Receiver for Xyne Spaces webhooks on the RADAR desk channel (Control
-- Center support tickets). v1 is deliberately ack-and-log: verify the HMAC,
-- dedup on the event's externalId, log what arrived, always 2xx — so Xyne
-- never retry-storms while the downstream consumer (e.g. notifying the
-- Control Center UI) is built on top later.
--
-- Uses the RADAR workspace credentials ('radarXyneCfg'), NOT the in-app
-- issue-desk webhook secret — the two Xyne apps sign independently.
module Domain.Action.UI.RadarXyneWebhook
  ( postRadarXyneWebhook,
  )
where

import qualified Data.Aeson as A
import qualified Data.Text as T
import Environment
import qualified Kernel.External.Ticket.XyneSpaces.Types as Xyne
import Kernel.External.Ticket.XyneSpaces.Webhook (RawByteString (..), verifyXyneSignature)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Error
import Kernel.Utils.Common

dedupTtlSeconds :: Int
dedupTtlSeconds = 7 * 24 * 3600

dedupKey :: Text -> Text
dedupKey externalId = "radar:xyne:webhook:dedup:" <> externalId

postRadarXyneWebhook :: Maybe Text -> RawByteString -> Flow APISuccess
postRadarXyneWebhook mbSig rawBody@(RawByteString bodyBytes) = do
  cfg <- asks (.radarXyneCfg)
  when (T.null (T.strip cfg.webhookSigningSecret)) $
    throwError $ InvalidRequest "RADAR Xyne webhook is not configured for this environment."
  sig <- fromMaybeM (InvalidRequest "MISSING_XYNE_SIGNATURE") mbSig
  verifyXyneSignature cfg.webhookSigningSecret sig rawBody
  -- Signature is valid from here on, so every path acks with 2xx: a body we
  -- cannot use is our problem to log, not Xyne's to retry.
  case A.eitherDecode @Xyne.XyneWebhookEvent bodyBytes of
    Left err -> logWarning $ "RADAR Xyne webhook body did not parse, acking anyway: " <> T.pack err
    Right event
      | event.eventType /= "DESK_REPLY" ->
        logInfo $ "RADAR Xyne webhook ignoring eventType=" <> event.eventType
      | otherwise -> do
        isFirst <- Redis.setNxExpire (dedupKey event.payload.externalId) dedupTtlSeconds True
        if not isFirst
          then logInfo $ "RADAR Xyne webhook duplicate delivery, externalId=" <> event.payload.externalId
          else
            logInfo $
              "RADAR desk reply received: ticketId="
                <> fromMaybe "<none>" event.payload.ticketId
                <> " threadId="
                <> event.payload.threadId
                <> " replier="
                <> fromMaybe "<unknown>" event.payload.replierName
                <> " attachments="
                <> show (maybe 0 length event.payload.attachments)
  pure Success
