{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Email.GCP.Flow
  ( sendEmail,
    sendMagicLinkEmail,
    sendBusinessVerificationEmail,
    sendEmailWithAttachment,
  )
where

import Data.Aeson (object, (.=))
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Email.Types as Email
import Kernel.Prelude
import Network.HTTP.Client (RequestBody (..), httpLbs, method, newManager, parseRequest, requestBody, requestHeaders, responseBody, responseStatus)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)

data SendGridEmail = SendGridEmail
  { personalizations :: [Personalization],
    from :: EmailAddress,
    subject :: Text,
    content :: [Content],
    attachments :: Maybe [Attachment]
  }
  deriving (Generic)

instance ToJSON SendGridEmail where
  toJSON email =
    object
      [ "personalizations" .= email.personalizations,
        "from" .= email.from,
        "subject" .= email.subject,
        "content" .= email.content,
        "attachments" .= email.attachments
      ]

data Personalization = Personalization
  { to :: [EmailAddress]
  }
  deriving (Generic)

instance ToJSON Personalization where
  toJSON p = object ["to" .= p.to]

data EmailAddress = EmailAddress
  { email :: Text,
    name :: Maybe Text
  }
  deriving (Generic)

instance ToJSON EmailAddress where
  toJSON addr =
    object $
      ["email" .= addr.email]
        <> maybe [] (\n -> ["name" .= n]) addr.name

data Content = Content
  { contentType :: Text,
    value :: Text
  }
  deriving (Generic)

instance ToJSON Content where
  toJSON c = object ["type" .= c.contentType, "value" .= c.value]

data Attachment = Attachment
  { attachmentContent :: Text,
    filename :: Text,
    attachmentType :: Text,
    disposition :: Text
  }
  deriving (Generic)

instance ToJSON Attachment where
  toJSON a =
    object
      [ "content" .= a.attachmentContent,
        "filename" .= a.filename,
        "type" .= a.attachmentType,
        "disposition" .= a.disposition
      ]

sendViaSendGrid :: String -> SendGridEmail -> IO ()
sendViaSendGrid apiUrl emailData = do
  manager <- newManager tlsManagerSettings
  initialRequest <- parseRequest apiUrl
  mbApiKey <- lookupEnv "SENDGRID_API_KEY"
  apiKey <- case mbApiKey of
    Just k -> return $ T.pack k
    Nothing -> error "SENDGRID_API_KEY environment variable not set"

  let request =
        initialRequest
          { method = "POST",
            requestHeaders =
              [ ("Authorization", "Bearer " <> TE.encodeUtf8 apiKey),
                ("Content-Type", "application/json")
              ],
            requestBody = RequestBodyLBS $ A.encode emailData
          }

  response <- httpLbs request manager
  let status = statusCode $ responseStatus response
  when (status < 200 || status >= 300) $ do
    let errBody = TE.decodeUtf8 $ BSL.toStrict $ responseBody response
    error $ "SendGrid API error (status " <> T.pack (show status) <> "): " <> errBody

buildEmail :: Text -> [Text] -> Text -> Text -> Maybe [Attachment] -> SendGridEmail
buildEmail from to subject body attachments =
  SendGridEmail
    { personalizations = [Personalization {to = map (\e -> EmailAddress e Nothing) to}],
      from = EmailAddress from Nothing,
      subject = subject,
      content = [Content "text/plain" body],
      attachments = attachments
    }

sendEmail :: String -> Email.EmailOTPConfig -> [Text] -> Text -> IO ()
sendEmail apiUrl config to otpCode = do
  let from = config.fromEmail
      subject = config.subject
      bodyText = T.replace "<otp>" otpCode config.bodyTemplate
      emailData = buildEmail from to subject bodyText Nothing
  sendViaSendGrid apiUrl emailData

sendMagicLinkEmail :: String -> Email.EmailMagicLinkConfig -> [Text] -> Text -> IO ()
sendMagicLinkEmail apiUrl config to token = do
  let from = config.fromEmail
      subject = config.subject
      verificationUrl = T.replace "<token>" token config.verificationUrlTemplate
      bodyText = T.replace "<link>" verificationUrl config.bodyTemplate
      emailData = buildEmail from to subject bodyText Nothing
  sendViaSendGrid apiUrl emailData

sendBusinessVerificationEmail :: String -> Email.EmailBusinessVerificationConfig -> [Text] -> Text -> Text -> IO ()
sendBusinessVerificationEmail apiUrl config to otpCode token = do
  let from = config.fromEmail
      subject = config.subject
      verificationUrl = T.replace "<token>" token config.verificationUrlTemplate
      bodyText = T.replace "<otp>" otpCode $ T.replace "<link>" verificationUrl config.bodyTemplate
      emailData = buildEmail from to subject bodyText Nothing
  sendViaSendGrid apiUrl emailData

sendEmailWithAttachment ::
  String ->  -- API URL
  Text ->    -- From email
  [Text] ->  -- To emails
  Text ->    -- Subject
  Text ->    -- Body text
  FilePath ->  -- PDF file path
  Text ->    -- Attachment filename
  IO ()
sendEmailWithAttachment apiUrl from to subject bodyText pdfPath fileName = do
  -- Check if file exists
  fileExists <- doesFileExist pdfPath
  unless fileExists $ do
    error ("Attachment file not found: " <> T.pack pdfPath)

  -- Read PDF file and encode to base64
  pdfContent <- BS.readFile pdfPath
  let pdfBase64 = TE.decodeUtf8 $ B64.encode pdfContent

  let attachment =
        Attachment
          { attachmentContent = pdfBase64,
            filename = fileName,
            attachmentType = "application/pdf",
            disposition = "attachment"
          }
      emailData = buildEmail from to subject bodyText (Just [attachment])

  sendViaSendGrid apiUrl emailData
