-- | Inbound webhook for the RADAR desk's Xyne app ("Control center tickets").
--
-- With the desk set to "Send replies to app webhook", Xyne delivers every
-- agent reply here and only sends it once we answer 2xx with the reply's
-- @externalId@. Control Center reads replies from Xyne itself (the ticket
-- conversation), so nothing is persisted: verify, dedup, log, ack.
--
-- The app signs with its own secret, kept in the city's
-- @Ticket_RadarXyneSpaces@ row ('XyneSpacesCfg.webhookSigningSecret'). One URL
-- serves every city: @mocId@ only names the row to read the secret from, and
-- all of a merchant's RADAR rows carry the same secret. There is no env
-- fallback, so no other Xyne app's secret is ever accepted here.
module Domain.Action.UI.RadarXyneWebhook (postRadarXyneWebhook) where

import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Domain.Types.MerchantServiceConfig as DMSC
import Environment
import EulerHS.Prelude hiding (id)
import qualified IssueManagement.Domain.Action.UI.XyneWebhook as XyneShared
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.Ticket.Interface.Types as Ticket
import qualified Kernel.External.Ticket.XyneSpaces.Types as XyneTypes
import Kernel.External.Ticket.XyneSpaces.Webhook (RawByteString (..), verifyXyneSignature)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import Storage.ConfigPilot.Config.MerchantServiceConfig (MerchantServiceConfigDimensions (..))

dedupTtlSeconds :: Int
dedupTtlSeconds = 7 * 24 * 60 * 60

dedupKey :: Text -> Text
dedupKey externalId = "radar:xyne:webhook:dedup:" <> externalId

-- | Just the event type, read before the full decode: only @DESK_REPLY@
-- carries the reply payload shape, other events would fail that decode.
newtype XyneEventType = XyneEventType {eventType :: Text}
  deriving stock (Generic)
  deriving anyclass (A.FromJSON)

postRadarXyneWebhook :: Maybe Text -> Maybe Text -> RawByteString -> Flow XyneShared.XyneWebhookAck
postRadarXyneWebhook mbMocId mbSig rawBody =
  withLogTag "radarXyneWebhook" $ do
    mocId <- mbMocId & fromMaybeM (InvalidRequest "mocId query param missing")
    signingSecret <- radarSigningSecret mocId
    sigHeader <- mbSig & fromMaybeM (InvalidRequest "X-Xyne-Signature header missing")
    verifyXyneSignature signingSecret sigHeader rawBody
    -- Authentic from here on: every outcome acks, so Xyne never retries a
    -- delivery we cannot use.
    case A.eitherDecode (getRawByteString rawBody) of
      Left err -> do
        logError $ "RADAR Xyne webhook: body parse failed, acking: " <> T.pack err
        pure noAck
      Right (XyneEventType "DESK_REPLY") -> case A.eitherDecode (getRawByteString rawBody) of
        Left err -> do
          logError $ "RADAR Xyne webhook: DESK_REPLY parse failed, acking: " <> T.pack err
          pure noAck
        Right (event :: XyneTypes.XyneWebhookEvent) -> handleDeskReply event.payload
      Right (XyneEventType other) -> do
        logInfo $ "RADAR Xyne webhook: ignoring eventType=" <> other
        pure noAck
  where
    noAck = XyneShared.XyneWebhookAck {externalId = ""}

handleDeskReply :: XyneTypes.XyneDeskReplyPayload -> Flow XyneShared.XyneWebhookAck
handleDeskReply payload = do
  isFirst <- Redis.setNxExpire (dedupKey payload.externalId) dedupTtlSeconds True
  if isFirst
    then
      logInfo $
        "RADAR desk reply received: ticketId="
          <> fromMaybe "<none>" payload.ticketId
          <> " threadId="
          <> payload.threadId
          <> " externalId="
          <> payload.externalId
          <> " replier="
          <> fromMaybe "<unknown>" payload.replierName
          <> " attachments="
          <> show (maybe 0 length payload.attachments)
    else logInfo $ "RADAR Xyne webhook: duplicate delivery, externalId=" <> payload.externalId
  pure XyneShared.XyneWebhookAck {externalId = payload.externalId}

-- | The RADAR app's signing secret from the city's @Ticket_RadarXyneSpaces@
-- row. A missing row or secret is reported to the caller as a bad signature,
-- the same answer the in-app webhook gives for an unknown @mocId@.
radarSigningSecret :: Text -> Flow Text
radarSigningSecret mocId = do
  mbMsc <-
    getOneConfig
      (MerchantServiceConfigDimensions {merchantOperatingCityId = mocId, merchantId = Nothing, serviceName = Just (DMSC.IssueTicketService Ticket.RadarXyneSpaces)})
      Nothing
  case mbMsc <&> (.serviceConfig) of
    Just (DMSC.IssueTicketServiceConfig (Ticket.RadarXyneSpacesConfig cfg)) ->
      case cfg.webhookSigningSecret of
        Just encryptedSecret -> decrypt encryptedSecret
        Nothing -> do
          logError $ "RADAR Xyne webhook: no webhookSigningSecret on the RADAR row for mocId=" <> mocId
          throwError (InvalidRequest "INVALID_XYNE_SIGNATURE")
    _ -> do
      logError $ "RADAR Xyne webhook: no RADAR config for mocId=" <> mocId
      throwError (InvalidRequest "INVALID_XYNE_SIGNATURE")
