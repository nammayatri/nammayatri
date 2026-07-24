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
import EulerHS.Types (base64Encode)
import qualified IssueManagement.Common as Common
import qualified IssueManagement.Common.Dashboard.Issue as DCI
import qualified IssueManagement.Domain.Action.Dashboard.Issue as DDI
import qualified IssueManagement.Domain.Action.UI.Issue as DUI
import qualified IssueManagement.Domain.Types.Issue.IssueReport as DIR
import qualified IssueManagement.Domain.Types.MediaFile as DMF
import IssueManagement.Storage.BeamFlow
import qualified IssueManagement.Storage.CachedQueries.MediaFile as CQMF
import qualified IssueManagement.Storage.Queries.Issue.IssueReport as QIR
import qualified IssueManagement.Storage.Queries.MediaFile as QMF
import IssueManagement.Tools.Error
import IssueManagement.Utils.Html (stripHtml)
import qualified IssueManagement.Utils.RemoteFile as RF
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
    Redis.HedisFlow m r,
    HasField "s3Env" r (S3.S3Env m)
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
    Redis.HedisFlow m r,
    HasField "s3Env" r (S3.S3Env m)
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
      merchantConfig <- issueHandle.findMerchantConfig merchantId mocId (Just $ cast issueReport.personId)
      mediaIds <-
        maybe
          (pure [])
          (mapM $ xyneAttachmentToMediaFile merchantConfig identifier issueReport.personId)
          payload.attachments
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

-- | Persist a Xyne attachment and rehost it on our own S3.
--
-- The row is created pointing at the third-party url first, then upgraded in
-- place once the copy lands. That ordering is deliberate: a working url already
-- exists, so starting from @PENDING@ (as the rider upload path does, where the
-- file genuinely is not available yet) would hide the attachment for the
-- duration of the fetch, and hide it permanently if the fetch failed. Creating
-- it ready and upgrading on success means a failed rehost degrades to the old
-- url-only behaviour instead of losing the attachment.
xyneAttachmentToMediaFile ::
  ( BeamFlow m r,
    HasField "s3Env" r (S3.S3Env m)
  ) =>
  Common.MerchantConfig ->
  Common.Identifier ->
  Id Common.Person ->
  Xyne.XyneAttachment ->
  m (Id DMF.MediaFile)
xyneAttachmentToMediaFile merchantConfig identifier personId att = do
  fid <- generateGUID
  now <- getCurrentTime
  let fileType = mimeTypeToFileType att.mimeType
      mf =
        DMF.MediaFile
          { DMF.id = fid,
            DMF._type = fileType,
            DMF.url = att.url,
            DMF.s3FilePath = Nothing,
            -- COMPLETED, not CONFIRMED: the readiness checks that gate media on
            -- the way out (toChatMessageItem, mkMediaFiles, recreateIssueChats)
            -- only accept COMPLETED or a null status, so anything else is
            -- silently filtered out of the chat response. There is no async
            -- upload to wait on here — the Xyne URL is stored as-is — so the
            -- row is ready the moment it is written.
            DMF.status = Just DMF.COMPLETED,
            DMF.fileHash = Nothing,
            DMF.name = Just att.name,
            DMF.size = att.size,
            DMF.createdAt = now,
            DMF.updatedAt = Just now
          }
  QMF.create mf
  fork "S3 Put Xyne Attachment" $ rehostToS3 merchantConfig identifier personId att fileType fid
  pure fid

-- | Copy a Xyne-hosted attachment into our own S3 and repoint the row at it.
--
-- Best effort by design: every failure path leaves the row untouched, still
-- serving the original third-party url, so a rehost problem degrades to the
-- previous behaviour rather than breaking the attachment.
rehostToS3 ::
  ( BeamFlow m r,
    HasField "s3Env" r (S3.S3Env m)
  ) =>
  Common.MerchantConfig ->
  Common.Identifier ->
  Id Common.Person ->
  Xyne.XyneAttachment ->
  S3.FileType ->
  Id DMF.MediaFile ->
  m ()
rehostToS3 merchantConfig identifier personId att fileType fid =
  handleFailure $ case mimeTypeToExtension att.mimeType of
    -- No sensible extension: leave it pointing at Xyne rather than storing a
    -- file we cannot label or serve correctly.
    Nothing ->
      logWarning $ "Xyne rehost skipped, unsupported mimeType=" <> fromMaybe "<none>" att.mimeType <> " mediaFileId=" <> fid.getId
    Just ext -> do
      -- Cheap pre-screen on the size Xyne advertises, before spending a fetch.
      if maybe False (> merchantConfig.mediaFileSizeUpperLimit) att.size
        then logWarning $ "Xyne rehost skipped, advertised size exceeds limit, mediaFileId=" <> fid.getId
        else
          RF.fetchRemoteFile att.url merchantConfig.mediaFileSizeUpperLimit >>= \case
            Nothing -> logWarning $ "Xyne rehost skipped, body exceeded size limit, mediaFileId=" <> fid.getId
            Just remote -> do
              filePath <- S3.createFilePath "issue-media/" ("xyne-" <> personId.getId) fileType ext
              S3.put (T.unpack filePath) (base64Encode remote.content)
              let fileUrl =
                    merchantConfig.mediaFileUrlPattern
                      & T.replace "<DOMAIN>" "issue"
                      & T.replace "<FILE_PATH>" filePath
              QMF.updateRehostedById fileUrl filePath fid
              -- Without this the pre-rehost row stays cached and clients keep
              -- being handed the third-party url until the entry expires.
              CQMF.clearMediaFileByIdCache identifier fid
              logInfo $ "Xyne attachment rehosted to " <> filePath
  where
    handleFailure action =
      action `catch` \(e :: SomeException) ->
        logError $ "Xyne rehost failed for mediaFileId=" <> fid.getId <> ", keeping original url: " <> show e

mimeTypeToFileType :: Maybe Text -> S3.FileType
mimeTypeToFileType Nothing = S3.PDF
mimeTypeToFileType (Just mt)
  | "image/" `T.isPrefixOf` mt = S3.Image
  | "audio/" `T.isPrefixOf` mt = S3.Audio
  | "video/" `T.isPrefixOf` mt = S3.Video
  | mt == "application/pdf" = S3.PDF
  | otherwise = S3.PDF

-- | Extension for the S3 object key. Deliberately separate from
-- 'validateContentType', which is the rider-upload allowlist and rejects
-- @application/pdf@ outright — agents can attach documents that customers
-- cannot upload. 'Nothing' means "do not rehost this".
mimeTypeToExtension :: Maybe Text -> Maybe Text
mimeTypeToExtension Nothing = Nothing
mimeTypeToExtension (Just mt) = case T.toLower (T.strip (T.takeWhile (/= ';') mt)) of
  "image/png" -> Just "png"
  "image/jpeg" -> Just "jpg"
  "image/jpg" -> Just "jpg"
  "image/gif" -> Just "gif"
  "image/webp" -> Just "webp"
  "image/heic" -> Just "heic"
  "image/heif" -> Just "heif"
  "application/pdf" -> Just "pdf"
  "audio/mpeg" -> Just "mp3"
  "audio/mp3" -> Just "mp3"
  "audio/mp4" -> Just "m4a"
  "audio/aac" -> Just "aac"
  "audio/wav" -> Just "wav"
  "audio/wave" -> Just "wav"
  "audio/webm" -> Just "webm"
  "audio/ogg" -> Just "ogg"
  "video/mp4" -> Just "mp4"
  "video/webm" -> Just "webm"
  _ -> Nothing

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
-- delivered at @/internal/xyne/webhook/bearer@.
processXyneBearerWebhook ::
  ( Esq.EsqDBReplicaFlow m r,
    EncFlow m r,
    BeamFlow m r
  ) =>
  Text ->
  DUI.ServiceHandle m ->
  Common.Identifier ->
  Maybe Text ->
  RawByteString ->
  m APISuccess
processXyneBearerWebhook bearerToken issueHandle identifier mbAuthHeader rawBody = do
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
        Just status -> do
          issueReport <-
            QIR.findByTicketIdOrAdditional event.payload.ticketId
              >>= fromMaybeM (IssueReportDoesNotExist event.payload.ticketId)
          merchantId <-
            issueReport.merchantId
              & fromMaybeM (InternalError $ "IssueReport " <> issueReport.id.getId <> " has no merchantId")
          mocId <-
            issueReport.merchantOperatingCityId
              & fromMaybeM (InternalError $ "IssueReport " <> issueReport.id.getId <> " has no merchantOperatingCityId")
          merchant <-
            issueHandle.findByMerchantId merchantId
              >>= fromMaybeM (MerchantDoesNotExist merchantId.getId)
          moCity <-
            issueHandle.findMOCityById mocId
              >>= fromMaybeM (MerchantOperatingCityNotFound mocId.getId)
          void $
            DDI.issueUpdate
              merchant.shortId
              moCity.city
              issueReport.id
              issueHandle
              identifier
              DCI.IssueUpdateByUserReq
                { status = Just status,
                  assignee = Nothing,
                  userId = cast issueReport.personId
                }
        Nothing -> do
          logError $ "Xyne bearer webhook: unrecognized status=" <> event.payload.status
          throwError (InvalidRequest "XYNE_BEARER_WEBHOOK_INVALID_STATUS")
    other ->
      logWarning $ "Xyne bearer webhook: ignoring unsupported eventType=" <> other
  pure Success
