{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Process inbound Xyne Spaces @DESK_REPLY@ webhooks.
--
-- The handler is shared between rider-app and driver-app. Each app supplies:
--
--   * the global @signingSecret@ (env var, paired with the @/internal/xyne/webhook@ URL),
--   * a @lookupXyneCfg@ that resolves the per-merchant @XyneSpacesCfg@ from
--     @MerchantServiceConfig@ (used for the fallback agent id and the
--     @convertHtmlToPlainText@ flag),
--   * the @dashboardIssueHandle@ and @Identifier@ for that app's reply context.
--
-- The handler then verifies the HMAC, dedups on @externalId@ via Redis, fetches
-- the IssueReport by @threadId@, optionally strips HTML, materialises Xyne
-- attachments as @MediaFile@ rows (URL-only — no S3 rehosting), and forwards to
-- 'sendDashboardChatMessage'. Driver-app's chat-notification slot stays
-- 'Nothing'; the chat row still persists.
module IssueManagement.Domain.Action.UI.XyneWebhook
  ( processXyneWebhook,
    processXyneBearerWebhook,
    XyneWebhookAck (..),
  )
where

import qualified AWS.S3 as S3
import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified IssueManagement.Common as Common
import qualified IssueManagement.Common.Dashboard.Issue as DCI
import qualified IssueManagement.Domain.Action.Dashboard.Issue as DDI
import qualified IssueManagement.Domain.Action.UI.Issue as DUI
import qualified IssueManagement.Domain.Types.Issue.IssueReport as DIR
import qualified IssueManagement.Domain.Types.MediaFile as DMF
import IssueManagement.Storage.BeamFlow
import qualified IssueManagement.Storage.Queries.Issue.IssueReport as QIR
import qualified IssueManagement.Storage.Queries.MediaFile as QMF
import IssueManagement.Tools.Error
import IssueManagement.Utils.Html (stripHtml)
import qualified Kernel.External.Ticket.XyneSpaces.Config as Xyne
import qualified Kernel.External.Ticket.XyneSpaces.Types as Xyne
import Kernel.External.Ticket.XyneSpaces.Webhook (RawByteString (..), verifyXyneSignature)
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto.Config as Esq
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.APISuccess (APISuccess (..))
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Text.Show as Show

-- | What we 2xx back to Xyne. The @externalId@ field correlates the reply with
-- our @ChatMessage.id@ so Xyne can store it against the message on their side.
newtype XyneWebhookAck = XyneWebhookAck
  { externalId :: Text
  }
  deriving stock (Generic)
  deriving anyclass (A.ToJSON, A.FromJSON, ToSchema)

instance Show.Show XyneWebhookAck where
  show ack = "XyneWebhookAck { externalId = " <> T.unpack ack.externalId <> " }"

dedupTtlSeconds :: Int
dedupTtlSeconds = 7 * 24 * 60 * 60

dedupKey :: Text -> Text
dedupKey externalId = "xyne:webhook:dedup:" <> externalId

processXyneWebhook ::
  ( Esq.EsqDBReplicaFlow m r,
    BeamFlow m r,
    CoreMetrics m,
    Redis.HedisFlow m r
  ) =>
  -- | Global signing secret paired with the @/internal/xyne/webhook@ URL.
  Text ->
  -- | Resolve the per-merchant Xyne config (token, agent fallback id, html-strip flag).
  (Id Common.Merchant -> Id Common.MerchantOperatingCity -> m Xyne.XyneSpacesCfg) ->
  -- | The app's dashboard service handle (same one used by @sendDashboardChatMessage@).
  DUI.ServiceHandle m ->
  -- | CUSTOMER for rider-app, DRIVER for driver-app.
  Common.Identifier ->
  -- | Value of @X-Xyne-Signature@ header.
  Maybe Text ->
  -- | Raw bytes of the request body.
  RawByteString ->
  m XyneWebhookAck
processXyneWebhook signingSecret lookupXyneCfg issueHandle identifier mbSigHeader rawBody =
  withLogTag "xyneWebhook" $ do
    sigHeader <- mbSigHeader & fromMaybeM (InvalidRequest "X-Xyne-Signature header missing")
    verifyXyneSignature signingSecret sigHeader rawBody
    event <- case A.eitherDecode (getRawByteString rawBody) of
      Right e -> pure (e :: Xyne.XyneWebhookEvent)
      Left err -> do
        logError $ "Xyne webhook: body parse failed: " <> T.pack err
        throwError (InvalidRequest "XYNE_WEBHOOK_PARSE_FAILED")
    case event.eventType of
      "DESK_REPLY" -> handleDeskReply lookupXyneCfg issueHandle identifier event.payload
      other -> do
        logError $ "Xyne webhook: ignoring unsupported eventType=" <> other
        -- 2xx so Xyne does not retry; nothing to correlate.
        pure $ XyneWebhookAck {externalId = ""}

handleDeskReply ::
  ( Esq.EsqDBReplicaFlow m r,
    BeamFlow m r,
    CoreMetrics m,
    Redis.HedisFlow m r
  ) =>
  (Id Common.Merchant -> Id Common.MerchantOperatingCity -> m Xyne.XyneSpacesCfg) ->
  DUI.ServiceHandle m ->
  Common.Identifier ->
  Xyne.XyneDeskReplyPayload ->
  m XyneWebhookAck
handleDeskReply lookupXyneCfg issueHandle identifier payload = do
  let key = dedupKey payload.externalId
  mbCached <- Redis.safeGet key
  case mbCached of
    Just cachedMsgId -> do
      logError $ "Xyne webhook: duplicate externalId=" <> payload.externalId <> " -> " <> cachedMsgId
      pure $ XyneWebhookAck {externalId = cachedMsgId}
    Nothing -> do
      let issueReportId = Id payload.threadId :: Id DIR.IssueReport
      issueReport <- QIR.findById issueReportId >>= fromMaybeM (IssueReportDoesNotExist payload.threadId)
      merchantId <-
        issueReport.merchantId
          & fromMaybeM (InternalError $ "IssueReport " <> payload.threadId <> " has no merchantId")
      mocId <-
        issueReport.merchantOperatingCityId
          & fromMaybeM (InternalError $ "IssueReport " <> payload.threadId <> " has no merchantOperatingCityId")
      cfg <- lookupXyneCfg merchantId mocId
      merchant <-
        issueHandle.findByMerchantId merchantId
          >>= fromMaybeM (MerchantDoesNotExist merchantId.getId)
      moCity <-
        issueHandle.findMOCityById mocId
          >>= fromMaybeM (MerchantOperatingCityNotFound mocId.getId)
      let messageText =
            if fromMaybe True cfg.convertHtmlToPlainText
              then stripHtml payload.body
              else payload.body
          senderUserIdText = fromMaybe cfg.xyneAgentUserId payload.replierUserId
      mediaIds <- maybe (pure []) (mapM xyneAttachmentToMediaFile) payload.attachments
      let req =
            DCI.SendChatMessageByUserReq
              { DCI.message = messageText,
                DCI.mediaFileIds = if null mediaIds then Nothing else Just mediaIds,
                DCI.userId = Id senderUserIdText
              }
      resp <-
        DDI.sendDashboardChatMessage
          merchant.shortId
          moCity.city
          issueReport.id
          issueHandle
          identifier
          req
      Redis.setExp key resp.messageId dedupTtlSeconds
      pure $ XyneWebhookAck {externalId = resp.messageId}

-- | Persist a Xyne attachment as a @MediaFile@ row that points at the
-- third-party URL directly. We do not rehost on our S3 yet — if Xyne URLs turn
-- out to be short-lived, a fetch + S3.put step can be added here without
-- changing the call site.
xyneAttachmentToMediaFile ::
  BeamFlow m r =>
  Xyne.XyneAttachment ->
  m (Id DMF.MediaFile)
xyneAttachmentToMediaFile att = do
  fid <- generateGUID
  now <- getCurrentTime
  let mf =
        DMF.MediaFile
          { DMF.id = fid,
            DMF._type = mimeTypeToFileType att.mimeType,
            DMF.url = att.url,
            DMF.s3FilePath = Nothing,
            DMF.createdAt = now
          }
  QMF.create mf
  pure fid

mimeTypeToFileType :: Maybe Text -> S3.FileType
mimeTypeToFileType Nothing = S3.PDF
mimeTypeToFileType (Just mt)
  | "image/" `T.isPrefixOf` mt = S3.Image
  | "audio/" `T.isPrefixOf` mt = S3.Audio
  | "video/" `T.isPrefixOf` mt = S3.Video
  | mt == "application/pdf" = S3.PDF
  | otherwise = S3.PDF

-- | Payload for a Xyne @TICKET_STATUS_UPDATED@ event, delivered to the
-- Bearer-token webhook. Independent of the DESK_REPLY event/payload used by
-- 'processXyneWebhook' — Xyne sends this as a separate notification shape.
data XyneTicketStatusUpdatedPayload = XyneTicketStatusUpdatedPayload
  { ticketId :: Text,
    xyneTicketId :: Text,
    conversationId :: Text,
    channelId :: Text,
    status :: Text,
    performedByUserId :: Text
  }
  deriving stock (Generic)
  deriving anyclass (A.FromJSON)

data XyneBearerWebhookEvent = XyneBearerWebhookEvent
  { eventType :: Text,
    payload :: XyneTicketStatusUpdatedPayload
  }
  deriving stock (Generic)
  deriving anyclass (A.FromJSON)

-- | Bearer-token webhook for Xyne @TICKET_STATUS_UPDATED@ notifications,
-- delivered at @/internal/xyne/webhook/bearer@. Deliberately self-contained:
-- it does not share code with 'processXyneWebhook' beyond the pre-existing
-- 'IssueManagement.Storage.Queries.Issue.IssueReport.updateIssueStatus' query,
-- so the HMAC-signature DESK_REPLY endpoint is never touched by changes here.
processXyneBearerWebhook ::
  BeamFlow m r =>
  -- | Global bearer token paired with the @/internal/xyne/webhook/bearer@ URL.
  Text ->
  -- | Value of the @Authorization@ header.
  Maybe Text ->
  -- | Raw bytes of the request body.
  RawByteString ->
  m APISuccess
processXyneBearerWebhook bearerToken mbAuthHeader rawBody = do
  unless (mbAuthHeader == Just ("Bearer " <> bearerToken)) $
    throwError $ AuthBlocked "Invalid Authorization header"
  event <- case A.eitherDecode (getRawByteString rawBody) of
    Right e -> pure (e :: XyneBearerWebhookEvent)
    Left err -> do
      logError $ "Xyne bearer webhook: body parse failed: " <> T.pack err
      throwError (InvalidRequest "XYNE_BEARER_WEBHOOK_PARSE_FAILED")
  case event.eventType of
    "TICKET_STATUS_UPDATED" ->
      case readMaybe (T.unpack (T.toUpper event.payload.status)) :: Maybe Common.IssueStatus of
        Just status -> QIR.updateIssueStatus event.payload.ticketId status
        Nothing -> logWarning $ "Xyne bearer webhook: unrecognized status=" <> event.payload.status
    other ->
      logWarning $ "Xyne bearer webhook: ignoring unsupported eventType=" <> other
  pure Success
