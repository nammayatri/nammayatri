-- | The driver app's one path for sending an email: attachments are fetched from URLs (a presigned S3 link,
-- or any URL a caller supplies), the message goes out through Email.Flow (SES or SendGrid), and the
-- provider's message id is returned so delivery and bounce events can be matched back to the send.
--
-- Callers: the internal notification webhook (plasma), TDS certificate disbursement.
module SharedLogic.EmailDelivery
  ( EmailRequest (..),
    EmailAttachmentRef (..),
    sendEmail,
  )
where

import qualified Control.Exception as E
import qualified Data.Text as T
import qualified Email.Flow as Email
import qualified IssueManagement.Utils.RemoteFile as RemoteFile
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common

-- | An attachment to download and attach. 'contentType' overrides the type the URL serves.
data EmailAttachmentRef = EmailAttachmentRef
  { url :: Text,
    filename :: Text,
    contentType :: Maybe Text
  }

data EmailRequest = EmailRequest
  { from :: Text,
    to :: [Text],
    subject :: Text,
    body :: Text,
    bodyFormat :: Email.EmailBodyFormat,
    attachments :: [EmailAttachmentRef],
    options :: Email.EmailSendOptions
  }

-- | Send the email. Returns the provider and message id, or Nothing for an untracked plain email (no
-- attachments and no tracking options), which keeps the provider's plain-email call.
sendEmail ::
  (MonadFlow m, MonadReader r m, HasField "emailServiceConfig" r Email.EmailServiceConfig) =>
  EmailRequest ->
  m (Maybe Email.EmailSendResult)
sendEmail req = do
  emailServiceConfig <- asks (.emailServiceConfig)
  attachments <- traverse (fetchAttachment emailServiceConfig.maxAttachmentBytes) req.attachments
  result <-
    liftIO $
      E.try @E.SomeException $
        if null attachments && req.options == Email.noEmailSendOptions
          then Nothing <$ Email.sendPlainEmail emailServiceConfig req.from req.to req.subject req.body req.bodyFormat
          else Just <$> Email.sendEmailWithAttachmentsTracked emailServiceConfig req.options req.from req.to req.subject req.body req.bodyFormat attachments
  case result of
    Left err -> throwError (InternalError $ "Email send failed: " <> show err)
    Right sendResult -> pure sendResult

fetchAttachment :: MonadFlow m => Int -> EmailAttachmentRef -> m Email.EmailAttachment
fetchAttachment maxBytes att = do
  mbFetched <- liftIO $ E.try @E.SomeException $ RemoteFile.fetchRemoteFile att.url maxBytes
  rf <- case mbFetched of
    Right (Just x) -> pure x
    Right Nothing -> throwError (InvalidRequest $ "Attachment exceeds " <> T.pack (show maxBytes) <> " bytes: " <> att.url)
    Left err -> throwError (InvalidRequest $ "Attachment fetch failed for " <> att.url <> ": " <> T.pack (show err))
  let resolvedCT = case att.contentType of
        Just ct -> ct
        Nothing -> if T.null rf.contentType then "application/octet-stream" else rf.contentType
  pure Email.EmailAttachment {content = rf.content, filename = att.filename, contentType = resolvedCT}
