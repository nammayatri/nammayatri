{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Email.AWS.Flow where

import qualified Amazonka as AWS
import Amazonka.SES
import Amazonka.SES.Lens (sendRawEmailResponse_messageId, sendRawEmail_configurationSetName, sendRawEmail_tags)
import Control.Lens ((.~), (^.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import Data.Char (isAscii)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Email.Types as Email
import Kernel.Prelude
import System.Directory (doesFileExist)

sendEmail :: Email.EmailOTPConfig -> [Text] -> Text -> IO ()
sendEmail config to otpCode = do
  env <- AWS.newEnv AWS.discover
  let from = config.fromEmail
      subject = config.subject
      textBody = T.replace "<otp>" otpCode config.bodyTemplate
      destination = Destination' Nothing Nothing (Just to)
      message = newMessage (newContent subject) (Body' Nothing (Just $ newContent textBody))
      sendReq = newSendEmail from destination message
  void $ AWS.runResourceT $ AWS.send env sendReq

-- | Send a plain subject+body email (no attachment, no template)
sendPlainEmail :: Text -> [Text] -> Text -> Text -> Email.EmailBodyFormat -> IO ()
sendPlainEmail from to subject bodyText bodyFormat = do
  env <- AWS.newEnv AWS.discover
  let destination = Destination' Nothing Nothing (Just to)
      body = case bodyFormat of
        Email.HtmlText -> Body' (Just (newContent bodyText)) (Just (newContent (htmlToPlaintext bodyText)))
        Email.Text -> Body' Nothing (Just (newContent bodyText))
      message = newMessage (newContent subject) body
      sendReq = newSendEmail from destination message
  void $ AWS.runResourceT $ AWS.send env sendReq

sendMagicLinkEmail :: Email.EmailMagicLinkConfig -> [Text] -> Text -> IO ()
sendMagicLinkEmail config to token = do
  env <- AWS.newEnv AWS.discover
  let from = config.fromEmail
      subject = config.subject
      verificationUrl = T.replace "<token>" token config.verificationUrlTemplate
      textBody = T.replace "<link>" verificationUrl config.bodyTemplate
      destination = Destination' Nothing Nothing (Just to)
      message = newMessage (newContent subject) (Body' Nothing (Just $ newContent textBody))
      sendReq = newSendEmail from destination message
  void $ AWS.runResourceT $ AWS.send env sendReq

sendBusinessVerificationEmail :: Email.EmailBusinessVerificationConfig -> [Text] -> Text -> Text -> IO ()
sendBusinessVerificationEmail config to otpCode token = do
  env <- AWS.newEnv AWS.discover
  let from = config.fromEmail
      subject = config.subject
      verificationUrl = T.replace "<token>" token config.verificationUrlTemplate
      textBody = T.replace "<otp>" otpCode $ T.replace "<link>" verificationUrl config.bodyTemplate
      destination = Destination' Nothing Nothing (Just to)
      message = newMessage (newContent subject) (Body' Nothing (Just $ newContent textBody))
      sendReq = newSendEmail from destination message
  void $ AWS.runResourceT $ AWS.send env sendReq

-- | Send email with PDF attachment using raw MIME format
sendEmailWithAttachment ::
  Text -> -- From email
  [Text] -> -- To emails
  Text -> -- Subject
  Text -> -- Body text
  FilePath -> -- PDF file path
  Text -> -- Attachment filename
  IO ()
sendEmailWithAttachment from to subject bodyText pdfPath fileName = do
  -- Check if file exists
  fileExists <- doesFileExist pdfPath
  unless fileExists $ do
    error ("Attachment file not found: " <> T.pack pdfPath)

  -- Read PDF file and encode to base64
  pdfContent <- BS.readFile pdfPath
  let pdfBase64 = TE.decodeUtf8 $ B64.encode pdfContent

  -- Build raw MIME email
  let rawEmail = buildRawEmail from to subject bodyText pdfBase64 fileName "application/pdf"

  -- Send via SES
  env <- AWS.newEnv AWS.discover
  let rawMessage = newRawMessage (TE.encodeUtf8 rawEmail)
      sendReq = newSendRawEmail rawMessage
  void $ AWS.runResourceT $ AWS.send env sendReq

-- | Build raw MIME email with PDF attachment
buildRawEmail ::
  Text -> -- From
  [Text] -> -- To
  Text -> -- Subject
  Text -> -- Body
  Text -> -- PDF Base64 content
  Text -> -- Filename
  Text -> -- MIME type
  Text
buildRawEmail from to subject body pdfBase64 fileName mimeType =
  let boundary = "----=_Part_0_123456789.987654321"
      toAddressList = T.intercalate ", " to
   in T.unlines
        [ "From: " <> from,
          "To: " <> toAddressList,
          "Subject: " <> subject,
          "MIME-Version: 1.0",
          "Content-Type: multipart/mixed; boundary=\"" <> boundary <> "\"",
          "",
          "--" <> boundary,
          "Content-Type: text/plain; charset=UTF-8",
          "Content-Transfer-Encoding: 7bit",
          "",
          body,
          "",
          "--" <> boundary,
          "Content-Type: " <> mimeType <> "; name=\"" <> fileName <> "\"",
          "Content-Description: " <> fileName,
          "Content-Disposition: attachment; filename=\"" <> fileName <> "\"",
          "Content-Transfer-Encoding: base64",
          "",
          pdfBase64,
          "",
          "--" <> boundary <> "--"
        ]

sendEmailWithAttachments ::
  Text ->
  [Text] ->
  Text ->
  Text ->
  Email.EmailBodyFormat ->
  [Email.EmailAttachment] ->
  IO ()
sendEmailWithAttachments from to subject bodyText bodyFormat attachments =
  void $ sendEmailWithAttachmentsTracked Email.noEmailSendOptions from to subject bodyText bodyFormat attachments

-- | Same message as 'sendEmailWithAttachments', sent with the given configuration set and message tags;
-- returns the SES MessageId so delivery and bounce events can be matched back to this send.
sendEmailWithAttachmentsTracked ::
  Email.EmailSendOptions ->
  Text ->
  [Text] ->
  Text ->
  Text ->
  Email.EmailBodyFormat ->
  [Email.EmailAttachment] ->
  IO Text
sendEmailWithAttachmentsTracked options from to subject bodyText bodyFormat attachments = do
  let rawEmail = buildMultiAttachmentRawEmail from to subject bodyText bodyFormat attachments
  env <- AWS.newEnv AWS.discover
  let rawMessage = newRawMessage (TE.encodeUtf8 rawEmail)
      sendReq =
        newSendRawEmail rawMessage
          & sendRawEmail_configurationSetName .~ options.configurationSet
          & sendRawEmail_tags .~ (if null options.tags then Nothing else Just (uncurry newMessageTag <$> options.tags))
  response <- AWS.runResourceT $ AWS.send env sendReq
  pure $ response ^. sendRawEmailResponse_messageId

htmlToPlaintext :: Text -> Text
htmlToPlaintext = T.strip . dropTags
  where
    dropTags s = case T.breakOn "<" s of
      (before, "") -> before
      (before, rest) -> case T.breakOn ">" rest of
        (_, "") -> before
        (_, gt) -> before <> " " <> dropTags (T.drop 1 gt)

buildMultiAttachmentRawEmail ::
  Text ->
  [Text] ->
  Text ->
  Text ->
  Email.EmailBodyFormat ->
  [Email.EmailAttachment] ->
  Text
buildMultiAttachmentRawEmail from to subject body bodyFormat attachments =
  let outerBoundary = "----=_Part_0_123456789.987654321"
      altBoundary = "----=_Alt_0_abcdefghi.jklmnopq"
      toAddressList = T.intercalate ", " to
      bodyPart = case bodyFormat of
        Email.HtmlText ->
          [ "--" <> outerBoundary,
            "Content-Type: multipart/alternative; boundary=\"" <> altBoundary <> "\"",
            "",
            "--" <> altBoundary,
            "Content-Type: text/plain; charset=UTF-8",
            "Content-Transfer-Encoding: 7bit",
            "",
            htmlToPlaintext body,
            "",
            "--" <> altBoundary,
            "Content-Type: text/html; charset=UTF-8",
            "Content-Transfer-Encoding: 7bit",
            "",
            body,
            "",
            "--" <> altBoundary <> "--",
            ""
          ]
        Email.Text ->
          [ "--" <> outerBoundary,
            "Content-Type: text/plain; charset=UTF-8",
            "Content-Transfer-Encoding: 7bit",
            "",
            body,
            ""
          ]
      attachmentPart att =
        let b64 = TE.decodeUtf8 . B64.encode $ att.content
         in [ "--" <> outerBoundary,
              "Content-Type: " <> att.contentType <> "; name=\"" <> att.filename <> "\"",
              "Content-Description: " <> att.filename,
              "Content-Disposition: attachment; filename=\"" <> att.filename <> "\"",
              "Content-Transfer-Encoding: base64",
              "",
              b64,
              ""
            ]
      headerPart =
        [ "From: " <> from,
          "To: " <> toAddressList,
          "Subject: " <> subject,
          "MIME-Version: 1.0",
          "Content-Type: multipart/mixed; boundary=\"" <> outerBoundary <> "\"",
          ""
        ]
      closing = ["--" <> outerBoundary <> "--"]
   in T.unlines $ headerPart <> bodyPart <> concatMap attachmentPart attachments <> closing

sendEncodedEmailWithAttachments ::
  Text ->
  [Text] ->
  Text ->
  Text ->
  Email.EmailBodyFormat ->
  [Email.EmailAttachment] ->
  IO ()
sendEncodedEmailWithAttachments from to subject bodyText bodyFormat attachments = do
  let rawEmail = buildEncodedAttachmentRawEmail from to subject bodyText bodyFormat attachments
  env <- AWS.newEnv AWS.discover
  void $ AWS.runResourceT $ AWS.send env (newSendRawEmail (newRawMessage (TE.encodeUtf8 rawEmail)))

buildEncodedAttachmentRawEmail ::
  Text ->
  [Text] ->
  Text ->
  Text ->
  Email.EmailBodyFormat ->
  [Email.EmailAttachment] ->
  Text
buildEncodedAttachmentRawEmail from to subject body bodyFormat attachments =
  let outerBoundary = "----=_Part_0_123456789.987654321"
      altBoundary = "----=_Alt_0_abcdefghi.jklmnopq"
      encodedTextPart contentType content =
        [ "Content-Type: " <> contentType <> "; charset=UTF-8",
          "Content-Transfer-Encoding: base64",
          "",
          base64Lines (TE.encodeUtf8 content),
          ""
        ]
      bodyPart = case bodyFormat of
        Email.HtmlText ->
          ["--" <> outerBoundary, "Content-Type: multipart/alternative; boundary=\"" <> altBoundary <> "\"", "", "--" <> altBoundary]
            <> encodedTextPart "text/plain" (htmlToPlaintext (dropHtmlBlock "title" (dropHtmlBlock "style" body)))
            <> ["--" <> altBoundary]
            <> encodedTextPart "text/html" body
            <> ["--" <> altBoundary <> "--", ""]
        Email.Text -> ["--" <> outerBoundary] <> encodedTextPart "text/plain" body
      attachmentPart att =
        [ "--" <> outerBoundary,
          "Content-Type: " <> att.contentType <> "; name=\"" <> att.filename <> "\"",
          "Content-Description: " <> att.filename,
          "Content-Disposition: attachment; filename=\"" <> att.filename <> "\"",
          "Content-Transfer-Encoding: base64",
          "",
          base64Lines att.content,
          ""
        ]
      headerPart =
        [ "From: " <> from,
          "To: " <> T.intercalate ", " to,
          "Subject: " <> encodeHeaderValue subject,
          "MIME-Version: 1.0",
          "Content-Type: multipart/mixed; boundary=\"" <> outerBoundary <> "\"",
          ""
        ]
      closing = ["--" <> outerBoundary <> "--"]
   in T.intercalate "\r\n" (headerPart <> bodyPart <> concatMap attachmentPart attachments <> closing) <> "\r\n"

dropHtmlBlock :: Text -> Text -> Text
dropHtmlBlock name content = case T.breakOn ("<" <> name) content of
  (before, "") -> before
  (before, rest) -> case T.breakOn closingTag rest of
    (_, "") -> before
    (_, closing) -> before <> dropHtmlBlock name (T.drop (T.length closingTag) closing)
  where
    closingTag = "</" <> name <> ">"

base64Lines :: BS.ByteString -> Text
base64Lines = T.intercalate "\r\n" . T.chunksOf 76 . TE.decodeUtf8 . B64.encode

encodeHeaderValue :: Text -> Text
encodeHeaderValue value
  | T.all isAscii value = value
  | otherwise = T.intercalate "\r\n " (map encodedWord (T.chunksOf 10 value))
  where
    encodedWord chunk = "=?UTF-8?B?" <> TE.decodeUtf8 (B64.encode (TE.encodeUtf8 chunk)) <> "?="
