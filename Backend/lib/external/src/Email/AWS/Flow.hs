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
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
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
